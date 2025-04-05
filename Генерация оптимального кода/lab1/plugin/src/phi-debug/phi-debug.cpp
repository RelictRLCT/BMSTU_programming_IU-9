#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

#include "tree.h"
#include "options.h"  
#include "flags.h"  
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-expr.h"
#include "gimple-ssa.h"
#include "tree-pass.h"
#include "tree-phinodes.h"
#include "tree-ssa-operands.h"
#include "tree-ssa-alias.h"
#include "ssa-iterators.h"
#include "context.h"

#include "plugin.h"
#include "plugin-version.h"

#include "alloc-pool.h"

#include <iostream>
#include <sstream>
#include <string>

#include "cfg.h" 
#include "basic-block.h"
#include "dumpfile.h"  
#include "gimple-pretty-print.h"



#define PREFIX_UNUSED(variable) ((void)variable) // Макрос, чтобы
// компилятор не ругался на неиспользуемые переменные.
int plugin_is_GPL_compatible = 1;
#define PLUGIN_NAME "phi-debug" // имя
#define PLUGIN_VERSION "1.0.0" // версия
// информация о плагине.
#define PLUGIN_HELP "this plugin shows:\n"\
	"* basic blocks;\n"\
	"* GIMPLE instructions:\n"\
	" - arithmetic operations;\n"\
	" - phi-functions;\n"\
	" - branches;\n"\
	" - memory operations;"
	
static struct plugin_info phi_debug_plugin_info = {
	.version = PLUGIN_VERSION,
	.help = PLUGIN_HELP,
};

static const struct pass_data phi_debug_pass_data = {
	.type = GIMPLE_PASS,
	.name = PLUGIN_NAME,
	.optinfo_flags = OPTGROUP_NONE,
	.tv_id = TV_NONE,
	.properties_required = PROP_gimple_any,
	.properties_provided = 0,
	.properties_destroyed = 0,
	.todo_flags_start = 0,
	.todo_flags_finish = 0,
};

struct phi_debug_pass : gimple_opt_pass {
	phi_debug_pass(gcc::context*ctx) : gimple_opt_pass(phi_debug_pass_data, ctx) {}
	virtual unsigned int execute(function*fun) override;
	virtual phi_debug_pass *clone() override { return this; }
};


static void manual_print_tree(FILE* mem, tree t) {
    if (!t) {
        fprintf(mem, "<nil>");
        return;
    }
    switch (TREE_CODE(t)) {
        case INTEGER_CST:
            fprintf(mem, "%ld", (long)int_cst_value(t));
            break;
        case SSA_NAME:
            fprintf(mem, "_%u", SSA_NAME_VERSION(t));
            break;
        case VAR_DECL:
        case PARM_DECL:
            fprintf(mem, "%s", get_name(t) ? get_name(t) : "<unnamed>");
            break;
        case FUNCTION_DECL:
            fprintf(mem, "%s", IDENTIFIER_POINTER(DECL_NAME(t)));
            break;
        case ARRAY_REF: {
            tree array = TREE_OPERAND(t, 0);
            tree index = TREE_OPERAND(t, 1);
            manual_print_tree(mem, array);
            fprintf(mem, "[");
            manual_print_tree(mem, index);
            fprintf(mem, "]");
            break;
        }
        case MEM_REF: {
            fprintf(mem, "*(");
            tree addr = TREE_OPERAND(t, 0);
            manual_print_tree(mem, addr);
            fprintf(mem, ")");
            break;
        }    
        default:
            fprintf(mem, "<expr:%s>", get_tree_code_name(TREE_CODE(t)));
    }
}


static void manual_print_gimple_stmt(FILE* mem, gimple* stmt) {
    switch (gimple_code(stmt)) {
        case GIMPLE_ASSIGN: {
            tree lhs = gimple_assign_lhs(stmt);
            enum tree_code rhs_code = gimple_assign_rhs_code(stmt);

            manual_print_tree(mem, lhs);
            fprintf(mem, " = ");

            if (rhs_code == PLUS_EXPR || rhs_code == MINUS_EXPR ||
                rhs_code == MULT_EXPR || rhs_code == TRUNC_DIV_EXPR) {
                tree rhs1 = gimple_assign_rhs1(stmt);
                tree rhs2 = gimple_assign_rhs2(stmt);
                manual_print_tree(mem, rhs1);

                switch(rhs_code) {
                    case PLUS_EXPR:  fprintf(mem, " + "); break;
                    case MINUS_EXPR: fprintf(mem, " - "); break;
                    case MULT_EXPR:  fprintf(mem, " * "); break;
                    case TRUNC_DIV_EXPR: fprintf(mem, " / "); break;
                    default: break;
                }

                manual_print_tree(mem, rhs2);
            } else {
                tree rhs = gimple_assign_rhs1(stmt);
                manual_print_tree(mem, rhs);
            }
            fprintf(mem, ";");
            break;
        }
        case GIMPLE_PHI: {
            const gphi* phi = as_a<const gphi*>(stmt);
            manual_print_tree(mem, gimple_phi_result(phi));
            fprintf(mem, " = PHI(");
            for (unsigned int i = 0; i < gimple_phi_num_args(phi); ++i) {
                tree arg = gimple_phi_arg_def(phi, i);
                basic_block src = gimple_phi_arg_edge(phi, i)->src;
                fprintf(mem, "[BB%d: ", src->index);
                manual_print_tree(mem, arg);
                fprintf(mem, "]");
                if (i != gimple_phi_num_args(phi) - 1)
                    fprintf(mem, ", ");
            }
            fprintf(mem, ");");
            break;
        }
        case GIMPLE_COND: {
            tree lhs = gimple_cond_lhs(stmt);
            tree rhs = gimple_cond_rhs(stmt);
            enum tree_code cond_code = gimple_cond_code(stmt);

            fprintf(mem, "if (");
            manual_print_tree(mem, lhs);

            switch (cond_code) {
                case LT_EXPR:  fprintf(mem, " < "); break;
                case LE_EXPR:  fprintf(mem, " <= "); break;
                case GT_EXPR:  fprintf(mem, " > "); break;
                case GE_EXPR:  fprintf(mem, " >= "); break;
                case EQ_EXPR:  fprintf(mem, " == "); break;
                case NE_EXPR:  fprintf(mem, " != "); break;
                default:       fprintf(mem, " ? "); break;
            }

            manual_print_tree(mem, rhs);
            fprintf(mem, ")");
            break;
        }
        case GIMPLE_CALL: {
            tree fn = gimple_call_fn(stmt);
            if (fn && TREE_CODE(fn) == ADDR_EXPR) {
                fn = TREE_OPERAND(fn, 0);
            }
            manual_print_tree(mem, fn);
            fprintf(mem, "(");
            for (unsigned int i = 0; i < gimple_call_num_args(stmt); i++) {
                manual_print_tree(mem, gimple_call_arg(stmt, i));
                if (i < gimple_call_num_args(stmt) - 1)
                    fprintf(mem, ", ");
            }
            fprintf(mem, ");");
            break;
        }
        case GIMPLE_RETURN: {
            fprintf(mem, "return");
            const greturn *ret_stmt = as_a<const greturn *>(stmt);
            tree retval = gimple_return_retval(ret_stmt);
            if (retval) {
                fprintf(mem, " ");
                manual_print_tree(mem, retval);
            }
            fprintf(mem, ";");
            break;
        }
        default:
            fprintf(mem, "<unknown stmt>;");
            break;
    }
    fprintf(mem, "\n");
}


static unsigned int phi_debug_function(function *fn)
{
    std::cout << "func: \"" << function_name(fn) << "\" {\n";

    basic_block bb;
    FOR_EACH_BB_FN(bb, fn)
    {
        std::cout << "  BB" << bb->index << ":\n";

        std::cout << "    predsessors: ";
        {
            edge e;
            edge_iterator ei;
            FOR_EACH_EDGE(e, ei, bb->preds)
                std::cout << "BB" << e->src->index << " ";
            std::cout << "\n";
        }

        std::cout << "    succsessors: ";
        {
            edge e;
            edge_iterator ei;
            FOR_EACH_EDGE(e, ei, bb->succs)
                std::cout << "BB" << e->dest->index << " ";
            std::cout << "\n";
        }
        
		    for (gimple_stmt_iterator gsi = gsi_start_bb(bb);
		         !gsi_end_p(gsi);
		         gsi_next(&gsi))
		    {
		        gimple *stmt = gsi_stmt(gsi);
		        std::cout << "      ";
		        
		        char* buf = nullptr;
				size_t size = 0;
		        FILE* mem = open_memstream(&buf, &size);
				if (mem) {
					//print_gimple_stmt(mem, stmt, 0, TDF_SLIM); // Автоматический вывод
				    manual_print_gimple_stmt(mem, stmt); // Для красивого вывода коммента
				    fclose(mem);
				    std::string comment; 
				    
				    // Распознавание типа инструкции
					if (gimple_code(stmt) == GIMPLE_PHI) {
						comment = ";; PHI-функция";
					}
					else if (gimple_code(stmt) == GIMPLE_COND) {
						comment = ";; Ветвление (if)";
					}
					else if (is_gimple_call(stmt)) {
						comment = ";; Вызов функции";
					}
					else if (is_gimple_assign(stmt)) {
						enum tree_code code = gimple_assign_rhs_code(stmt);
						if (code == PLUS_EXPR || code == MINUS_EXPR ||
							code == MULT_EXPR || code == TRUNC_DIV_EXPR) {
							comment = ";; Арифметическая операция ";
						}
						
						tree lhs = gimple_assign_lhs(stmt);
						tree rhs = gimple_assign_rhs1(stmt);

						if (TREE_CODE(lhs) == ARRAY_REF || TREE_CODE(lhs) == MEM_REF ||
							TREE_CODE(rhs) == ARRAY_REF || TREE_CODE(rhs) == MEM_REF) {
							comment += ";; Доступ к памяти (ArrayRef/MemRef)";
						}
					}
					
					if (size > 0 && buf[size - 1] == '\n') {
						buf[size - 1] = '\0';
					}
					fprintf(stdout, "%-26s %s\n", buf, comment.c_str());
    				free(buf);
				}	
		    }
    }

    std::cout << "}\n\n";
    return 0;
}

unsigned int phi_debug_pass::execute(function*fn) { return phi_debug_function(fn); }

static struct register_pass_info phi_debug_pass_info = {
	.pass = new phi_debug_pass(g), // тот самый глобальный контекст
	.reference_pass_name = "ssa", // Прикрепимся к ssa-пассу
	.ref_pass_instance_number = 1, // 1 прогонка
	.pos_op = PASS_POS_INSERT_AFTER, // После ssa-пасса.
};

int plugin_init(struct plugin_name_args *args, struct plugin_gcc_version *version) {
	printf("Hello, GCC!!!\n");
	
	if(!plugin_default_version_check(version, &gcc_version)) { // Проверка версии
		return 1;
	}
	register_callback(args->base_name, PLUGIN_INFO, NULL, &phi_debug_plugin_info);
	register_callback(args->base_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &phi_debug_pass_info);

	return 0;
}	
