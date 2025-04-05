#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <fstream>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

LLVMContext Context;
IRBuilder<> Builder(Context);
std::unique_ptr<Module> module;
std::map<std::string, AllocaInst*> NamedValues;
std::istringstream Input;
std::string CurTok;

// Лексер
std::string next() {
    char c;
    CurTok = "";

    // Пробелы
    while (Input.get(c) && isspace(c));
    if (!Input) return "";

    CurTok += c;

    // Составные токены
    if (c == '=' || c == ':' || c == '>') {
        while (Input.peek() == '=' || Input.peek() == ':' || Input.peek() == '>' || Input.peek() == '(' || Input.peek() == ')')
            CurTok += Input.get();
    }
    // "+mod"
    else if (c == '+') {
        if (Input.peek() == 'm') {
            std::string mod;
            mod += Input.get(); // m
            mod += Input.get(); // o
            mod += Input.get(); // d
            if (mod == "mod") CurTok += mod;
            else {
                // неверный токен, надо вернуть символы 
                for (int i = mod.size() - 1; i >= 0; --i)
                    Input.putback(mod[i]);
            }
        }
    }
    // Идентификатор или ключевое слово
    else if (isalpha(c)) {
        while (isalnum(Input.peek()))
            CurTok += Input.get();
    }
    // Числа
    else if (isdigit(c) || c == '-') {
        while (isdigit(Input.peek()))
            CurTok += Input.get();
    }

    return CurTok;
}


bool match(const std::string &tok) { 
	return CurTok == tok ? (next(), true) : false; 
}


void initLexer(const std::string &src) { Input.clear(); Input.str(src); next(); }


Value* parsePrimary() {
    Value* val = nullptr;
    if (isdigit(CurTok[0]) || (CurTok[0] == '-' && isdigit(CurTok[1]))) {
        int num = std::stoi(CurTok);
        next();
        val = Builder.getInt32(num);
    } else if (isalpha(CurTok[0])) {
        std::string var = CurTok;
        next();
        if (NamedValues.find(var) == NamedValues.end()) {
            std::cerr << "Переменная не объявлена: " << var << "\n";
            exit(1);
        }
        val = Builder.CreateLoad(Builder.getInt32Ty(), NamedValues[var], var);
    } else {
        std::cerr << "Неожиданный токен: " << CurTok << "\n";
        exit(1);
    }
    return val;
}


Value* parseExpr() {
    Value* LHS = parsePrimary();

    if (CurTok == "+mod" || CurTok == "+" || CurTok == "-" || CurTok == ">" || CurTok == "<") {
        std::string op = CurTok;
        next();
        Value* RHS = parsePrimary(); 

        if (!RHS) {
            std::cerr << "Нету правой части выражения\n";
            exit(1);
        }

        if (op == "+mod") return Builder.CreateXor(LHS, RHS, "xored");
        if (op == "+") return Builder.CreateAdd(LHS, RHS, "addtmp");
        if (op == "-") return Builder.CreateSub(LHS, RHS, "subtmp");
        if (op == ">") return Builder.CreateICmpSGT(LHS, RHS, "cmptmp");
        if (op == "<") return Builder.CreateICmpSLT(LHS, RHS, "cmptmp");
    }

    return LHS;
}


void parseAssign() {
    std::string var = CurTok; next();
    match("=");

    Value *val = parseExpr();

    // Если переменная еще не объявлена
    if (NamedValues.find(var) == NamedValues.end()) {
        NamedValues[var] = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, var);
    }

    Builder.CreateStore(val, NamedValues[var]);
}


void parseBlock(Function *F, const std::string &endToken);


void parseIf(Function *F) {
    Value *cond = parseExpr();
    match("==>"); match("::(");

    BasicBlock *ThenBB = BasicBlock::Create(Context, "then", F);
    BasicBlock *MergeBB = BasicBlock::Create(Context, "ifcont", F);

    Builder.CreateCondBr(cond, ThenBB, MergeBB);
    Builder.SetInsertPoint(ThenBB);

    parseBlock(F, "::)"); // Подвыраженя в if

    Builder.CreateBr(MergeBB);
    Builder.SetInsertPoint(MergeBB);
}


void parseWhile(Function *F) {
	Value *cond = parseExpr();
    match("===>"); match(":::(");

    BasicBlock *CondBB = BasicBlock::Create(Context, "loopcond", F);
    BasicBlock *LoopBB = BasicBlock::Create(Context, "loopbody", F);
    BasicBlock *AfterBB = BasicBlock::Create(Context, "loopend", F);

    Builder.CreateBr(CondBB);
    Builder.SetInsertPoint(CondBB);

    Builder.CreateCondBr(cond, LoopBB, AfterBB);

    Builder.SetInsertPoint(LoopBB);
    
    parseBlock(F, ":::)"); // Подвыраженя в while
    
    Builder.CreateBr(CondBB);
    Builder.SetInsertPoint(AfterBB);
}


bool parseReturn() {
    match("return");
    Value *retVal = parseExpr();
    Builder.CreateRet(retVal);
    return true; 
}


void parseBlock(Function *F, const std::string &endToken) {
    while (!CurTok.empty() && CurTok != endToken) {
        if (CurTok == "if") {
            next(); parseIf(F);
        } else if (CurTok == "while") {
            next(); parseWhile(F);
        } else if (CurTok == "return") {
            parseReturn();
            break;  // После return выход
        } else {
            parseAssign();
        }
    }
    match(endToken);
}


void parseMain() {
    match("main"); match("=>"); match(":(");
    FunctionType *FT = FunctionType::get(Type::getInt32Ty(Context), false);
    Function *F = Function::Create(FT, Function::ExternalLinkage, "main", module.get());
    BasicBlock *BB = BasicBlock::Create(Context, "entry", F);
    Builder.SetInsertPoint(BB);

    while (!CurTok.empty() && CurTok != ":)") {
        if (CurTok == "if") { next(); parseIf(F); }
        else if (CurTok == "while") { next(); parseWhile(F); }
        else if (CurTok == "return") { 
            parseReturn(); 
            break; // Выход после return
        }
        else { parseAssign(); }
    }

    match(":)");
    
    BasicBlock *CurBB = Builder.GetInsertBlock();
    if (!CurBB->getTerminator()) {
        Builder.CreateRet(Builder.getInt32(0));
    }
    
    if (verifyFunction(*F, &llvm::errs())) { // надо было ещё передать поток ошибок
        fprintf(stderr, "Ошибка верификации функции!\n");
    }
    
    //printf("%d \n", verifyFunction(*F));
}


std::string readFile(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        throw std::runtime_error("Ошибка открытия файла: " + filename);
    }

    std::ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}


int main() {
    module = std::make_unique<Module>("lab3_module", Context);

    std::string code = readFile("../text.txt");

    initLexer(code);
    parseMain();

    module->print(llvm::outs(), nullptr);
    return 0;
}
