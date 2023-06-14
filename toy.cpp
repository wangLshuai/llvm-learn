#include <cctype>
#include <cstdio>
#include <llvm-10/llvm/IR/Value.h>
#include <llvm-10/llvm/Support/TargetSelect.h>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>

#include "KaleidoscopeJIT.h"

using namespace llvm;
using namespace llvm::orc;

class PrototypeAST;
static void InitializeModuleAndPassManager();
static std::unique_ptr<LLVMContext> TheContext;
static std::unique_ptr<IRBuilder<>> Builder;
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

// PrototypeAST - This class represents the "prototype" for a function,
// which captures its name, and its argument names (thus implicitly the number
// of arguments the function takes).
class PrototypeAST {
  std::string Name;

public:
  std::vector<std::string> Args;
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
      : Name(Name), Args(std::move(Args)) {}
  const std::string &getName() const { return Name; }
  Function *codegen();
};

Value *LogErrorV(const char *Str);

Function *getFunction(std::string Name) {
  if (auto *F = TheModule->getFunction(Name))
    return F;

  // if not, check whether we can codegen the declaration from some existing
  // prototype.
  auto FI = FunctionProtos.find(Name);
  if (FI != FunctionProtos.end())
    return FI->second->codegen();

  // If no existing progotype exists, return null
  return nullptr;
}

enum Token {
  tok_eof = -1,

  // commands
  tok_def = -2,
  tok_extern = -3,

  // primary
  tok_identifier = -4,
  tok_number = -5,
  tok_if = -6,
  tok_then = -7,
  tok_else = -8,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

// gettok - Return the next token from standard input.
static int gettok() {
  static int LastChar = ' ';

  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
    IdentifierStr = LastChar;
    while (isalnum(LastChar = getchar()))
      IdentifierStr += LastChar;

    // printf("token %s\n", IdentifierStr.c_str());
    if (IdentifierStr == "def")
      return tok_def;

    if (IdentifierStr == "extern")
      return tok_extern;

    if (IdentifierStr == "if")
      return tok_if;

    if (IdentifierStr == "then")
      return tok_then;

    if (IdentifierStr == "else")
      return tok_else;

    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();

    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), 0);
    return tok_number;
  }

  if (LastChar == '#') {
    // Comment until end of line;
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  // Check for end of file. Don't eat the EOF.
  if (LastChar == EOF)
    return tok_eof;

  // Otherwise, just return the character as its ascii value. +-*/(){},;
  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}

// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
  virtual ~ExprAST() = default;
  virtual Value *codegen() = 0;
};

// NumberExpAST - Expression class for numeric literals like "1.0".
class NumberExprAST : public ExprAST {
  double Val;

public:
  NumberExprAST(double Val) : Val(Val) {}
  Value *codegen() override;
};

Value *NumberExprAST::codegen() {
  return ConstantFP::get(*TheContext, APFloat(Val));
}

// VariableExprAST - Expression class for referecing a variable. like "a".
class VariableExprAST : public ExprAST {
  std::string Name;

public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
  Value *codegen() override;
};

Value *VariableExprAST::codegen() {
  Value *V = NamedValues[Name];
  if (!V)
    LogErrorV("Unknown variable name");
  return V;
}

// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
  char Op;
  std::unique_ptr<ExprAST> LHS, RHS;

public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
                std::unique_ptr<ExprAST> RHS)
      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  Value *codegen() override;
};

Value *BinaryExprAST::codegen() {
  Value *L = LHS->codegen();
  Value *R = RHS->codegen();

  if (!L || !R)
    return nullptr;

  switch (Op) {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
  default:
    return LogErrorV("invalid binay operator");
  }
}

// IfexprAST - Expression class for if/then/else
class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> Cond, Then, Else;

public:
  IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
            std::unique_ptr<ExprAST> Else)
      : Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}

  Value *codegen() override;
};

Value *IfExprAST::codegen() {

  Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;

  // Convert condition to a bool by comparing non-equal to 0.0
  CondV = Builder->CreateFCmpONE(
      CondV, ConstantFP::get(*TheContext, APFloat(0.0)), "ifcond");

  Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases. Insert the 'then' block at the
  // end of the function.
  BasicBlock *ThenBB = BasicBlock::Create(
      *TheContext, "then",
      TheFunction); // push_back ?heFunction->getBasicBlockList().push_back
  BasicBlock *ElseBB = BasicBlock::Create(*TheContext, "else", TheFunction);
  BasicBlock *MergeBB = BasicBlock::Create(*TheContext, "ifcont", TheFunction);

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);

  Value *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB); // branch jump, not condition
  // Codegen of 'Then' can chage the current block, update ThenBB for the PHI.
  ThenBB = Builder->GetInsertBlock();

  // Emit else block.
  // TheFunction->getBasicBlockList().push_back(ElseBB);
  Builder->SetInsertPoint(ElseBB);

  Value *ElseV = Else->codegen();
  if (!ElseV)
    return nullptr;

  Builder->CreateBr(MergeBB);
  // codegen of 'Else' can change the current block,update ElseBB for the PHI.
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block.
  // TheFunction->getBasicBlockList().push_back(MergeBB);
  Builder->SetInsertPoint(MergeBB);
  PHINode *PN = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;

public:
  CallExprAST(const std::string &Callee,
              std::vector<std::unique_ptr<ExprAST>> Args)
      : Callee(Callee), Args(std::move(Args)) {}
  Value *codegen() override;
};

Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  Function *CalleeF = getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unknown function referenced");

  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect $ arguments passed");

  std::vector<Value *> ArgsV;
  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
  // Make the function type: double(double,double) etc.
  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));

  FunctionType *FT =
      FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

  Function *F =
      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

  // Set names for all arguments.
  // unsigned Idx = 0;
  // for (auto &Arg : F->args()) {
  //   Arg.setName(Args[Idx++]);
  // }

  return F;
}

// FunctionAST - This class represents a function definititon itself.
class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;

public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
              std::unique_ptr<ExprAST> Body)
      : Proto(std::move(Proto)), Body(std::move(Body)) {}

  Function *codegen();
};

Function *FunctionAST::codegen() {
  // First, check for an existing funtion from a previoous 'extern'
  // declaration.
  auto &P = *Proto;
  FunctionProtos[Proto->getName()] = std::move(Proto);
  Function *TheFunction = getFunction(P.getName());

  if (!TheFunction)
    return nullptr;

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedValues map.
  NamedValues.clear();

  // Set names for all arguments.
  unsigned Idx = 0;
  for (auto &Arg : TheFunction->args()) {
    Arg.setName(P.Args[Idx++]);
    NamedValues[std::string(Arg.getName())] = &Arg;
  }

  if (Value *RetVal = Body->codegen()) {
    // Finish off the function
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    verifyFunction(*TheFunction);

    // Optimize the function.
    TheFPM->run(*TheFunction);

    return TheFunction;
  }

  // Error reading body,remove function from symbol table
  TheFunction->eraseFromParent();
  return nullptr;
}

// CurTok /getNextToken - Provide a simple token buffer. CurTok is the current
// token the parser is looking at. getNextToken reads another token from the
// lexer and updates CurTok with its results.

static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

// numberexpr :: = number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken();
  return Result;
}

static std::unique_ptr<ExprAST> ParseExpression();

// parenexpr ::= '(' expression ')'

static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;

  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ).
  return V;
}

// identifierexpr
// ::= identifier
// ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken(); // eat identifier.

  if (CurTok != '(')
    return std::make_unique<VariableExprAST>(IdName);

  // Call.
  getNextToken(); // eat (
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression())
        Args.push_back(std::move(Arg));
      else
        return nullptr;

      if (CurTok == ')')
        break;

      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  getNextToken(); // eat ')'
  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken(); // eat the if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;

  if (CurTok != tok_then)
    return LogError("expected then");

  getNextToken(); // eat the then

  auto Then = ParseExpression();
  if (!Then)
    return nullptr;

  if (CurTok != tok_else)
    return LogError("expected else");
  getNextToken(); // eat the else
  auto Else = ParseExpression();
  if (!Else)
    return nullptr;

  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
                                     std::move(Else));
}

// primary
// ::= identifierexpr
// ::= numberexpr
// ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  default:
    return LogError("unknown token when expecting an expression");
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  }
}

// BinopPrecedence - This holds the precedence for each binary operator that is
// defined.
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Get the precedence of the pening  binary operator token.
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

// binoprhs
// ::=('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {

  // If this is a binop,find its precedence.
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;

    // Okay, we know this is a binop.
    int BinOp = CurTok;
    getNextToken(); // eat binop

    // Parse the primay expression after the binary operator.
    auto RHS = ParsePrimary();
    if (!RHS)
      return nullptr;

    // If BinOp binds less tightly with RHS thab the operator after RHS,let
    // the pending operator take RHS as its LHS.
    int NexPrec = GetTokPrecedence();
    if (TokPrec < NexPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS =
        std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

// expression
// ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParsePrimary();
  if (!LHS)
    return nullptr;

  return ParseBinOpRHS(0, std::move(LHS));
}

// prototype
// ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  if (CurTok != tok_identifier)
    return LogErrorP("expected function name in prototype");

  std::string FnName = IdentifierStr;
  getNextToken();

  if (CurTok != '(')
    return LogErrorP("Exprected '(' in prototype");

  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier)
    ArgNames.push_back(IdentifierStr);
  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  // success.
  getNextToken(); // eat ')'
  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

// defination ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken(); // eat def.
  auto Proto = ParsePrototype();
  if (!Proto)
    return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern;
  return ParsePrototype();
}

// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr",
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}
//===========-----------------------------------------------------------------------------------------=======//
// Top-level parsing
static void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition:");
      FnIR->print(errs());
      TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();
      //  FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    getNextToken();
  }
}
static void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *IR = ProtoAST->codegen()) {
      fprintf(stderr, "Read ProtoType:");
      IR->print(errs());
      fprintf(stderr, "\n");
      FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
    }
  } else {
    getNextToken();
  }
}

static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      // JIT the module containing the anonymous expression, keeping a handle so
      // we can free it later.
      auto H = TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();

      // Search the JIT for the __anon_expr_symbol.
      auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
      assert(ExprSymbol && "Function not found");

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      double (*FP)() =
          (double (*)())(intptr_t)cantFail(ExprSymbol.getAddress());
      fprintf(stderr, "Evaluated to %f\n", FP());

      // Delete the anonymous expression module from the JIT.
      TheJIT->removeModule(H);
    }
  } else {
    getNextToken();
  }
}
// top ::= definition | external | expression |';'
static void MainLoop() {
  while (true) {
    fprintf(stderr, "ready> ");
    switch (CurTok) {
    case tok_eof:
      return;
    case ';': // ignore top-level semicolons.
      getNextToken();
      break;
    case tok_def:
      HandleDefinition();
      break;
    case tok_extern:
      HandleExtern();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

// Top-level parsing and JIT Driver
static void InitializeModuleAndPassManager() {
  // Open a new context and moudle.
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("my cool jit", *TheContext);
  TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());

  // Create a new pass manager attached to it.
  TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

  // Do simple "peephole" optimizations and bit-twiddling optzns.
  TheFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->add(createReassociatePass());
  // Eliminate Common SubExpression.
  TheFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->add(createCFGSimplificationPass());

  TheFPM->doInitialization();

  // Create a new builder for the module.
  Builder = std::make_unique<IRBuilder<>>(*TheContext);
}

// ========-----------------------------------------=================//
// "Library" functions that can be "extern'd from user code"

extern "C" double putchard(double X) {
  fputc((char)X, stderr);
  return 0;
}

extern "C" double printd(double X) {
  fprintf(stderr, "%f\n", X);
  return 0;
}

int main() {
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
  TheJIT = std::make_unique<KaleidoscopeJIT>();
  InitializeModuleAndPassManager();

  // Install standard binary operators.
  // 1 is lowest precedence.
  BinopPrecedence['<'] = 10;
  BinopPrecedence['+'] = 20;
  BinopPrecedence['-'] = 20;
  BinopPrecedence['*'] = 40;

  // Prime the first token.
  fprintf(stderr, "ready> ");
  getNextToken();
  MainLoop();
  return 0;
}