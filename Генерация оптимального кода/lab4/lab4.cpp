#include <iostream>
#include <sstream>
#include <map>
#include <vector>
#include <fstream>
#include <unordered_set>
#include <set>


#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;


// Структура для хранения инструкций
struct IRInstr {
    std::string op;
    std::string dest;
    std::string arg1;
    std::string arg2;
};

// Базовый блок
struct CFGBlock {
    std::string name;
    std::vector<IRInstr> instructions;
    std::vector<CFGBlock*> successors;
};

// Control Flow Graph
struct CFG {
    std::vector<std::unique_ptr<CFGBlock>> blocks;
    CFGBlock *entry;

    CFG() : entry(nullptr) {}

    CFGBlock* createBlock(const std::string &name) {
        blocks.push_back(std::make_unique<CFGBlock>());
        blocks.back()->name = name;
        if (!entry) entry = blocks.back().get();
        return blocks.back().get();
    }
};

CFG cfg;
CFGBlock *curCFG = nullptr;

LLVMContext Context;
IRBuilder<> Builder(Context);
std::unique_ptr<Module> module;
std::map<std::string, AllocaInst*> NamedValues;
std::istringstream Input;
std::string CurTok;
std::string LastText;

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
        LastText = std::to_string(num);
        next();
        val = Builder.getInt32(num);
    } else if (isalpha(CurTok[0])) {
        std::string var = CurTok;
        LastText = var;
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
    Value *LHS = parsePrimary();
    std::string lhsText = LastText;

    if (CurTok=="+"||CurTok=="-"||CurTok=="+mod"||CurTok==">"||CurTok=="<") {
        std::string op = CurTok;
        next();
        
        Value *RHS = parsePrimary();
        
        if (!RHS) {
			printf("Нету правой части\n");
			exit(1);
		}
        
        std::string rhsText = LastText;

        Value *res = nullptr;
        std::string name;
        if (op == "+") {
            name = "addtmp";
            res  = Builder.CreateAdd(LHS, RHS, name);
            curCFG->instructions.push_back({"add", name, lhsText, rhsText});
        }
        else if (op == "-") {
            name = "subtmp";
            res  = Builder.CreateSub(LHS, RHS, name);
            curCFG->instructions.push_back({"sub", name, lhsText, rhsText});
        }
        else if (op == "+mod") {
            name = "xored";
            res  = Builder.CreateXor(LHS, RHS, name);
            curCFG->instructions.push_back({"xor", name, lhsText, rhsText});
        }
        else if (op == ">") {
            name = "cmptmp";
            res  = Builder.CreateICmpSGT(LHS, RHS, name);
            curCFG->instructions.push_back({"cmp", name, lhsText, rhsText});
        }
        else { // op == "<"
            name = "cmptmp";
            res  = Builder.CreateICmpSLT(LHS, RHS, name);
            curCFG->instructions.push_back({"cmp", name, lhsText, rhsText});
        }

        return res;
    }

    return LHS;
}


void parseAssign() {
    std::string var = CurTok; next();
    match("=");
	
	std::string rhsText = CurTok;
    Value *val = parseExpr();
    
    // Если это константа
    if (auto *C = dyn_cast<ConstantInt>(val)) {
        rhsText = std::to_string(C->getSExtValue());
    }
    // Если у value есть имя
    else if (val->hasName()) {
        rhsText = val->getName().str();
    }

    // Если переменная еще не объявлена
    if (NamedValues.find(var) == NamedValues.end()) {
        NamedValues[var] = Builder.CreateAlloca(Builder.getInt32Ty(), nullptr, var);
    }

    Builder.CreateStore(val, NamedValues[var]);
    curCFG->instructions.push_back({"store", var, rhsText, ""});
}


void parseBlock(Function *F, const std::string &endToken);


void parseIf(Function *F) {
    Value *cond = parseExpr();
    IRInstr &last = curCFG->instructions.back();
    std::string condText = last.dest;

    match("==>"); match("::(");

    BasicBlock *ThenBB = BasicBlock::Create(Context, "then", F);
    BasicBlock *MergeBB = BasicBlock::Create(Context, "ifcont", F);

    // CFG-блоки
    CFGBlock *thenCFG = cfg.createBlock("then");
    CFGBlock *mergeCFG = cfg.createBlock("ifcont");

    curCFG->instructions.push_back({"if_cond", "", condText, ""});

    curCFG->successors.push_back(thenCFG);
    curCFG->successors.push_back(mergeCFG);

    Builder.CreateCondBr(cond, ThenBB, MergeBB);

    Builder.SetInsertPoint(ThenBB);
    curCFG = thenCFG;
    curCFG->successors.push_back(mergeCFG);
    parseBlock(F, "::)");

    Builder.CreateBr(MergeBB);

    Builder.SetInsertPoint(MergeBB);
    curCFG = mergeCFG;
}


void parseWhile(Function *F) {
    
    BasicBlock *CondBB  = BasicBlock::Create(Context, "loopcond", F);
    BasicBlock *LoopBB  = BasicBlock::Create(Context, "loopbody", F);
    BasicBlock *AfterBB = BasicBlock::Create(Context, "loopend", F);
	
	// CFG-блоки
    CFGBlock *condCFG  = cfg.createBlock("loopcond");
    CFGBlock *loopCFG  = cfg.createBlock("loopbody");
    CFGBlock *afterCFG = cfg.createBlock("loopend");

    // Переход из curCFG в Cond
    curCFG->successors.push_back(condCFG);
    Builder.CreateBr(CondBB);

    Builder.SetInsertPoint(CondBB);
    curCFG = condCFG;
    
    Value *cond = parseExpr();      
    IRInstr &last = curCFG->instructions.back();
    std::string condText = last.dest;
    
    curCFG->instructions.push_back({"while_cond","",condText,""});
    
    match("===>");
    match(":::(");

    // Ветвление
    Builder.CreateCondBr(cond, LoopBB, AfterBB);
    condCFG->successors.push_back(loopCFG);
    condCFG->successors.push_back(afterCFG);

    // Вход в LoopBB 
    Builder.SetInsertPoint(LoopBB);
    curCFG = loopCFG;
    parseBlock(F, ":::)");

    Builder.CreateBr(CondBB);
    loopCFG->successors.push_back(condCFG);

    // После цикла
    Builder.SetInsertPoint(AfterBB);
    curCFG = afterCFG;
}



bool parseReturn() {
    match("return");
    
    Value *retVal = parseExpr();
    std::string retText = LastText;
    
    Builder.CreateRet(retVal);
    curCFG->instructions.push_back({"ret", "", retText, ""});
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
    CFGBlock    *entryCFG = cfg.createBlock("entry");
    curCFG = entryCFG;
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
        curCFG->instructions.push_back({"ret", "", "0", ""});
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


//  cfgPreds[b] - список предков
static std::unordered_map<CFGBlock*, std::vector<CFGBlock*>> cfgPreds;

static void buildPredList(CFG &cfg) {
    cfgPreds.clear();
    for (auto &bp : cfg.blocks) {
        CFGBlock *B = bp.get();
        for (CFGBlock *S : B->successors)
            cfgPreds[S].push_back(B);
    }
}

static std::unordered_map<std::string,int> versionCnt;
static std::unordered_map<std::string,std::vector<std::string>> stackVer;

static std::string curr(const std::string &var) {
    auto &st = stackVer[var];
    return st.empty() ? var + "0" : st.back();
}

// Переименование	
static void rename(CFGBlock *B,
                      std::unordered_set<CFGBlock*> &visited) {
    if (visited.count(B)) return;
    visited.insert(B);

    std::vector<std::string> defs;

	// Переименовывание фи-функций
    for (auto &I : B->instructions) {
        if (I.op == "phi") {
            std::string var = I.dest;
            int &cnt = versionCnt[var];
            std::string newName = var + std::to_string(++cnt);
            stackVer[var].push_back(newName);
            I.dest = newName;
            defs.push_back(var);
        }
    }

    // Обычные инструкции
    for (auto &I : B->instructions) {
        if (I.op != "phi") {
            if (!I.arg1.empty() && isalpha(I.arg1[0]))
                I.arg1 = curr(I.arg1);
            if (!I.arg2.empty() && isalpha(I.arg2[0]))
                I.arg2 = curr(I.arg2);

            if (!I.dest.empty() && isalpha(I.dest[0])) {
                std::string var = I.dest;
                int &cnt = versionCnt[var];
                std::string newName = var + std::to_string(++cnt);
                stackVer[var].push_back(newName);
                I.dest = newName;
                defs.push_back(var);
            }
        }
    }

    // Аргументы фи-функций 
    for (CFGBlock *succ : B->successors) {
        for (auto &I : succ->instructions) {
            if (I.op == "phi") {
                std::string baseVar = I.dest.substr(0, I.dest.find_first_of("0123456789"));
                std::string argVersion = curr(baseVar);

                if (I.arg1.empty())
                    I.arg1 = argVersion;
                else
                    I.arg1 += "," + argVersion;
            }
        }
    }

    // Рекурсивно в потомков
    for (CFGBlock *succ : B->successors)
        rename(succ, visited);

    // Откат stackVer на выходе
    for (auto it = defs.rbegin(); it != defs.rend(); ++it) {
        stackVer[*it].pop_back();
    }
}


std::map<CFGBlock*, std::set<CFGBlock*>> domFrontiers;

static std::unordered_map<CFGBlock*, CFGBlock*> idom;

// reverse‑postorder list и индекс в нём
static std::vector<CFGBlock*>             rpoList;
static std::unordered_map<CFGBlock*,int>  rpoIndex;

static CFGBlock* intersect(CFGBlock *b1, CFGBlock *b2) {
    CFGBlock *finger1 = b1, *finger2 = b2;

    while (finger1 != finger2) {
        while (rpoIndex[finger1] > rpoIndex[finger2])
            finger1 = idom[finger1];
        while (rpoIndex[finger2] > rpoIndex[finger1])
            finger2 = idom[finger2];
    }
    return finger1;
}

// DFS для сбора reverse‑postorder
static void dfsRPO(CFGBlock *B, std::unordered_set<CFGBlock*> &vis) {
    vis.insert(B);
    for (CFGBlock *S : B->successors)
        if (!vis.count(S))
            dfsRPO(S, vis);
    rpoList.push_back(B);
}


// заполняет глобальную map idom[B] для каждого блока B
void computeIDoms(CFG &cfg) {
    buildPredList(cfg);

    // Сборка всех блоков в rpoList (reverse‑postorder)
    rpoList.clear();
    std::unordered_set<CFGBlock*> visited;
    dfsRPO(cfg.entry, visited);
    std::reverse(rpoList.begin(), rpoList.end());

    rpoIndex.clear();
    for (int i = 0; i < (int)rpoList.size(); ++i)
        rpoIndex[rpoList[i]] = i;

    // Инициализация
    idom.clear();
    for (auto &bp : cfg.blocks)
        idom[bp.get()] = nullptr;
    // Entry доминирует сам над собой
    idom[cfg.entry] = cfg.entry;

    bool changed = true;
    while (changed) {
        changed = false;

        for (CFGBlock *B : rpoList) {
            if (B == cfg.entry) continue;

            // Первый предок с вычисленным idom
            CFGBlock *new_idom = nullptr;
            for (CFGBlock *P : cfgPreds[B]) {
                if (idom[P]) {
                    new_idom = P;
                    break;
                }
            }
            if (!new_idom) continue; 

            // для остальных предков - intersect
            for (CFGBlock *P : cfgPreds[B]) {
                if (P == new_idom) continue;
                if (idom[P])
                    new_idom = intersect(P, new_idom);
            }

            // если изменилось, надо прогнать ещё
            if (idom[B] != new_idom) {
                idom[B] = new_idom;
                changed = true;
            }
        }
    }
}

// Dominance Frontiers
void computeDominanceFrontiers(CFG &cfg) {
    domFrontiers.clear();
    for (auto &b : cfg.blocks) {
        CFGBlock *block = b.get();
        if (cfgPreds[block].size() >= 2) {
            for (auto *p : cfgPreds[block]) {
                CFGBlock *runner = p;
                while (runner != idom[block]) {
                    domFrontiers[runner].insert(block);
                    runner = idom[runner];
                    if (!runner) break;
                }
            }
        }
    }
}

void insertPhiFunctions(CFG &cfg) {
    buildPredList(cfg);
    computeIDoms(cfg);              // idoms
    computeDominanceFrontiers(cfg); // dominance frontiers

    std::map<std::string, std::set<CFGBlock*>> defBlocks;

    // Собираем блоки, где переменная определяется
    for (auto &bp : cfg.blocks)
        for (auto &I : bp->instructions)
            if (!I.dest.empty() && isalpha(I.dest[0]))
                defBlocks[I.dest.substr(0, I.dest.find_first_of("0123456789"))].insert(bp.get());

    // Вставка фи-функций по dominance frontier
    for (auto &[var, blocks] : defBlocks) {
        std::set<CFGBlock*> workset = blocks;
        std::set<CFGBlock*> done;

        while (!workset.empty()) {
            CFGBlock *B = *workset.begin();
            workset.erase(B);

            for (auto *Y : domFrontiers[B]) {
                bool alreadyHasPhi = false;
                for (auto &I : Y->instructions)
                    if (I.op == "phi" && I.dest == var)
                        alreadyHasPhi = true;

                if (!alreadyHasPhi) {
                    IRInstr phi;
                    phi.op = "phi";
                    phi.dest = var;
                    phi.arg1 = ""; // аргументы поставит rename
                    phi.arg2 = "";
                    Y->instructions.insert(Y->instructions.begin(), phi);
                    if (done.insert(Y).second)
                        workset.insert(Y);
                }
            }
        }
    }
}


static void toSSA(CFG &cfg) {
    insertPhiFunctions(cfg);
    std::unordered_set<CFGBlock*> v;
    rename(cfg.entry, v);
}


static std::string escape(const std::string &s) {
  std::string r;
  for (char c : s) {
    if      (c=='<') r += "&lt;";
    else if (c=='>') r += "&gt;";
    else if (c=='&') r += "&amp;";
    else             r += c;
  }
  return r;
}

void exportCFGtoDot(const CFG &cfg, const std::string &file) {
    std::ofstream dot(file);
    dot<<"digraph CFG {\n"
       <<"  node [shape=record,fontname=\"Courier\",fontsize=10];\n\n";

    for (auto &bp : cfg.blocks) {
      auto &B = *bp;
      std::ostringstream lbl;
      lbl<<"{"<<escape(B.name)<<"|";

      for (auto &I : B.instructions) {
        if (I.op == "store") {
          lbl<<escape(I.dest)<<" = store "<<escape(I.arg1);
        }
        else if (I.op=="add"||I.op=="sub"||I.op=="xor"||I.op=="cmp") {
          lbl<<escape(I.dest)<<" = "<<I.op<<" "
             <<escape(I.arg1)<<" "<<escape(I.arg2);
        }
        else if (I.op == "phi") {
          std::istringstream iss(I.arg1);
          std::string v;
          bool first = true;
          lbl<<escape(I.dest)<<" = φ(";
          while (std::getline(iss, v, ',')) {
            if (!first) lbl<<", ";
            first = false;
            lbl<<escape(v);
          }
          lbl<<")";
        }
        else if (I.op=="if_cond") {
          lbl<<"if "<<escape(I.arg1);
        }
        else if (I.op=="while_cond") {
          lbl<<"while "<<escape(I.arg1);
        }
        else if (I.op=="ret") {
          lbl<<"ret "<<escape(I.arg1);
        }

        lbl<<"\\l";
      }

      lbl<<"}";

      dot<<"  \""<<B.name<<"\" [label=\""<<lbl.str()<<"\"];\n";

      for (auto *s : B.successors)
        dot<<"  \""<<B.name<<"\" -> \""<<s->name<<"\";\n";
      dot<<"\n";
    }

    dot<<"}\n";
}




int main() {
    module = std::make_unique<Module>("lab4_module", Context);

    std::string code = readFile("../text.txt");

    initLexer(code);
    parseMain();

    module->print(llvm::outs(), nullptr);
    
    exportCFGtoDot(cfg, "../DOT/cfg.dot");
    
    toSSA(cfg);
    
    exportCFGtoDot(cfg, "../DOT/ssa.dot");
    return 0;
}
