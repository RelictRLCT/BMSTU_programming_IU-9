#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace llvm;

int main() {
  //Создал модуль и контекст
  LLVMContext Context;
  std::unique_ptr<Module> ModulePtr = std::make_unique<Module>("lab2_module", Context);
  Module *M = ModulePtr.get();
	
  //Определил тип функции main	
  FunctionType *MainFuncType = FunctionType::get(Type::getInt32Ty(Context), false);
  Function *MainFunc = Function::Create(MainFuncType, Function::ExternalLinkage, "main", M);
	
  //Базовый блок и IRBuilder		
  BasicBlock *Entry = BasicBlock::Create(Context, "entry", MainFunc);
  IRBuilder<> Builder(Entry);
	
  //Результат
  Value *Result = Builder.getInt32(353 + 48);
  Builder.CreateRet(Result);
  
  M->print(outs(), nullptr);

  return 0;
}
