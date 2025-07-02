/* Copyright (C) 2016 Jeremiah Orians
 * This file is part of stage0.
 *
 * stage0 is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * stage0 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with stage0.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "lisp.h"

/* Cognitive Grammar primitive function declarations */
struct cell* prim_test_cognitive(struct cell* args);
struct cell* prim_make_node(struct cell* args);
struct cell* prim_node_p(struct cell* args);
struct cell* prim_node_type(struct cell* args);
struct cell* prim_node_value(struct cell* args);
struct cell* prim_make_edge(struct cell* args);
struct cell* prim_edge_p(struct cell* args);
struct cell* prim_make_agent(struct cell* args);
struct cell* prim_agent_p(struct cell* args);
struct cell* prim_make_tensor_shape(struct cell* args);
struct cell* prim_hypergraph(struct cell* args);

/* Support functions */
struct cell* findsym(char *name)
{
	struct cell* symlist;
	for(symlist = all_symbols; nil != symlist; symlist = symlist->cdr)
	{
		if(!strcmp(name, symlist->car->string))
		{
			return symlist;
		}
	}
	return nil;
}

struct cell* make_sym(char* name);

struct cell* intern(char *name)
{
	struct cell* op = findsym(name);
	if(nil != op) return op->car;
	op = make_sym(name);
	all_symbols = make_cons(op, all_symbols);
	return op;
}

/*** Environment ***/
struct cell* extend(struct cell* env, struct cell* symbol, struct cell* value)
{
	return make_cons(make_cons(symbol, value), env);
}

struct cell* multiple_extend(struct cell* env, struct cell* syms, struct cell* vals)
{
	if(nil == syms)
	{
		return env;
	}
	return multiple_extend(extend(env, syms->car, vals->car), syms->cdr, vals->cdr);
}

struct cell* extend_env(struct cell* sym, struct cell* val, struct cell* env)
{
	env->cdr = make_cons(env->car, env->cdr);
	env->car = make_cons(sym, val);
	return val;
}

struct cell* assoc(struct cell* key, struct cell* alist)
{
	if(nil == alist) return nil;
	for(; nil != alist; alist = alist->cdr)
	{
		if(alist->car->car->string == key->string) return alist->car;
	}
	return nil;
}

/*** Evaluator (Eval/Apply) ***/
struct cell* eval(struct cell* exp, struct cell* env);
struct cell* make_proc(struct cell* a, struct cell* b, struct cell* env);
struct cell* evlis(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;
	return make_cons(eval(exps->car, env), evlis(exps->cdr, env));
}

struct cell* progn(struct cell* exps, struct cell* env)
{
	if(exps == nil) return nil;

	for(;;)
	{
		struct cell* result;
		result = eval(exps->car, env);
		if(exps->cdr == nil) return result;
		exps = exps->cdr;
	}
}

struct cell* apply(struct cell* proc, struct cell* vals)
{
	struct cell* temp = nil;
	if(proc->type == PRIMOP)
	{
		temp = (*(proc->function))(vals);
	}
	else if(proc->type == PROC)
	{
		struct cell* env = make_cons(proc->env->car, proc->env->cdr);
		temp = progn(proc->cdr, multiple_extend(env, proc->car, vals));
	}
	else
	{
		fprintf(stderr, "Bad argument to apply\n");
		exit(EXIT_FAILURE);
	}
	return temp;
}

struct cell* evcond(struct cell* exp, struct cell* env)
{
	/* Return nil but the result is technically undefined per the standard */
	if(nil == exp)
	{
		return nil;
	}

	if(tee == eval(exp->car->car, env))
	{
		return eval(exp->car->cdr->car, env);
	}

	return evcond(exp->cdr, env);
}

struct cell* process_sym(struct cell* exp, struct cell* env);
struct cell* process_cons(struct cell* exp, struct cell* env);

struct cell* eval(struct cell* exp, struct cell* env)
{
	if(exp == nil) return nil;

	switch(exp->type)
	{
		case INT: return exp;
		case SYM: return process_sym(exp, env);
		case CONS: return process_cons(exp, env);
		case PRIMOP: return exp;
		case PROC: return exp;
		default: return exp;
	}
	/* Not reached */
	return exp;
}

struct cell* process_sym(struct cell* exp, struct cell* env)
{
	struct cell* tmp = assoc(exp, env);
	if(tmp == nil)
	{
		fprintf(stderr,"Unbound symbol\n");
		exit(EXIT_FAILURE);
	}
	return tmp->cdr;
}

struct cell* process_if(struct cell* exp, struct cell* env)
{
	if(eval(exp->cdr->car, env) != nil)
	{
		return eval(exp->cdr->cdr->car, env);
	}
	return eval(exp->cdr->cdr->cdr->car, env);

}

struct cell* process_setb(struct cell* exp, struct cell* env)
{
	struct cell* newval = eval(exp->cdr->cdr->car, env);
	struct cell* pair = assoc(exp->cdr->car, env);
	pair->cdr = newval;
	return newval;
}

struct cell* process_let(struct cell* exp, struct cell* env)
{
	for(struct cell* lets = exp->cdr->car; lets != nil; lets = lets->cdr)
	{
		env = make_cons(make_cons(lets->car->car, eval(lets->car->cdr->car, env)), env);
	}
	return progn(exp->cdr->cdr, env);
}

struct cell* process_cons(struct cell* exp, struct cell* env)
{
	if(exp->car == s_if) return process_if(exp, env);
	if(exp->car == s_cond) return evcond(exp->cdr, env);
	if(exp->car == s_begin) return progn(exp->cdr, env);
	if(exp->car == s_lambda) return make_proc(exp->cdr->car, exp->cdr->cdr, env);
	if(exp->car == quote) return exp->cdr->car;
	if(exp->car == s_define) return(extend_env(exp->cdr->car, eval(exp->cdr->cdr->car, env), env));
	if(exp->car == s_setb) return process_setb(exp, env);
	if(exp->car == s_let) return process_let(exp, env);
	return apply(eval(exp->car, env), evlis(exp->cdr, env));
}


/*** Primitives ***/
struct cell* nullp(struct cell* args)
{
	if(nil == args->car) return tee;
	return nil;
}

struct cell* make_int(int a);
struct cell* prim_sum(struct cell* args)
{
	if(nil == args) return nil;

	int sum;
	for(sum = 0; nil != args; args = args->cdr)
	{
		sum = sum + args->car->value;
	}
	return make_int(sum);
}

struct cell* prim_sub(struct cell* args)
{
	if(nil == args) return nil;

	int sum = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		 sum = sum - args->car->value;
	}
	return make_int(sum);
}

struct cell* prim_prod(struct cell* args)
{
	if(nil == args) return nil;

	int prod;
	for(prod = 1; nil != args; args = args->cdr)
	{
		prod = prod * args->car->value;
	}
	return make_int(prod);
}

struct cell* prim_div(struct cell* args)
{
	if(nil == args) return make_int(1);

	int div = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		div = div / args->car->value;
	}
	return make_int(div);
}

struct cell* prim_mod(struct cell* args)
{
	if(nil == args) return nil;

	int mod = args->car->value % args->cdr->car->value;
	if(nil != args->cdr->cdr)
	{
		printf("wrong number of arguments to mod\n");
		exit(EXIT_FAILURE);
	}
	return make_int(mod);
}

struct cell* prim_and(struct cell* args)
{
	if(nil == args) return nil;

	for(; nil != args; args = args->cdr)
	{
		if(tee != args->car) return nil;
	}
	return tee;
}

struct cell* prim_or(struct cell* args)
{
	if(nil == args) return nil;

	for(; nil != args; args = args->cdr)
	{
		if(tee == args->car) return tee;
	}
	return nil;
}

struct cell* prim_not(struct cell* args)
{
	if(nil == args) return nil;

	if(tee != args->car) return tee;
	return nil;
}

struct cell* prim_numgt(struct cell* args)
{
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp <= args->car->value)
		{
			return nil;
		}
		temp = args->car->value;
	}
	return tee;
}

struct cell* prim_numge(struct cell* args)
{
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp < args->car->value)
		{
			return nil;
		}
		temp = args->car->value;
	}
	return tee;
}

struct cell* prim_numeq(struct cell* args)
{
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp != args->car->value)
		{
			return nil;
		}
	}
	return tee;
}

struct cell* prim_numle(struct cell* args)
{
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp > args->car->value)
		{
			return nil;
		}
		temp = args->car->value;
	}
	return tee;
}

struct cell* prim_numlt(struct cell* args)
{
	if(nil == args) return nil;

	int temp = args->car->value;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(temp >= args->car->value)
		{
			return nil;
		}
		temp = args->car->value;
	}
	return tee;
}

struct cell* prim_listp(struct cell* args)
{
	if(nil == args) return nil;

	if(CONS == args->car->type)
	{
		return tee;
	}
	return nil;
}

struct cell* prim_output(struct cell* args, FILE* out)
{
	for(; nil != args; args = args->cdr)
	{
		if(INT == args->car->type)
		{
			fprintf(out, "%d", args->car->value);
		}
		else if(ASCII == args->car->type)
		{
			fprintf(out, "%c", args->car->value);
		}
		else if(CONS == args->car->type)
		{
			prim_output(args->car, out);
		}
		else
		{
			fprintf(out, "%s", args->car->string);
		}
	}
	return tee;
}

struct cell* prim_stringeq(struct cell* args)
{
	if(nil == args) return nil;

	char* temp = args->car->string;
	for(args = args->cdr; nil != args; args = args->cdr)
	{
		if(strcmp(temp, args->car->string))
		{
			return nil;
		}
	}
	return tee;
}

struct cell* prim_display(struct cell* args)
{
	return prim_output(args, stdout);
}

struct cell* prim_write(struct cell* args)
{
	return prim_output(args, output);
}

int64_t cells_remaining();
struct cell* prim_freecell(struct cell* args)
{
	if(nil == args)
	{
		printf("Remaining Cells: ");
	}
	return make_int(cells_remaining());
}

struct cell* prim_ascii(struct cell* args)
{
	struct cell* temp;
	for(temp = args; nil != temp; temp = temp->cdr)
	{
		if(INT == temp->car->type)
		{
			temp->car->type = ASCII;
		}
	}
	return args;
}

struct cell* prim_halt(struct cell* args)
{
	fclose(output);
	exit(EXIT_SUCCESS);
}

/* Ghost in the Guile Shell - Cognitive Grammar Primitives */

/* Simple test function */
struct cell* prim_test_cognitive(struct cell* args)
{
	return make_sym("cognitive-test-works");
}

/* Hypergraph node: (node type value metadata) */
struct cell* prim_make_node(struct cell* args)
{
	if(nil == args || nil == args->cdr) return nil;
	
	struct cell* node_marker = make_sym("node");
	struct cell* type = args->car;
	struct cell* value = args->cdr->car;
	struct cell* metadata = (nil != args->cdr->cdr) ? args->cdr->cdr->car : nil;
	
	return make_cons(node_marker, 
		   make_cons(type, 
			   make_cons(value, 
				   make_cons(metadata, nil))));
}

/* Check if object is a hypergraph node */
struct cell* prim_node_p(struct cell* args)
{
	if(nil == args) return nil;
	
	struct cell* obj = args->car;
	if(nil == obj || SYM != obj->car->type) return nil;
	
	char* marker = obj->car->string;
	if(0 == strncmp(marker, "node", 4)) return tee;
	return nil;
}

/* Get node type */
struct cell* prim_node_type(struct cell* args)
{
	if(nil == args) return nil;
	
	struct cell* node = args->car;
	if(nil == node || nil == node->cdr) return nil;
	
	return node->cdr->car;
}

/* Get node value */
struct cell* prim_node_value(struct cell* args)
{
	if(nil == args) return nil;
	
	struct cell* node = args->car;
	if(nil == node || nil == node->cdr || nil == node->cdr->cdr) return nil;
	
	return node->cdr->cdr->car;
}

/* Hypergraph edge: (edge from to label weight) */
struct cell* prim_make_edge(struct cell* args)
{
	if(nil == args || nil == args->cdr || nil == args->cdr->cdr) return nil;
	
	struct cell* edge_marker = make_sym("edge");
	struct cell* from = args->car;
	struct cell* to = args->cdr->car;
	struct cell* label = args->cdr->cdr->car;
	struct cell* weight = (nil != args->cdr->cdr->cdr) ? args->cdr->cdr->cdr->car : make_int(1);
	
	return make_cons(edge_marker,
		   make_cons(from,
			   make_cons(to,
				   make_cons(label,
					   make_cons(weight, nil)))));
}

/* Check if object is a hypergraph edge */
struct cell* prim_edge_p(struct cell* args)
{
	if(nil == args) return nil;
	
	struct cell* obj = args->car;
	if(nil == obj || SYM != obj->car->type) return nil;
	
	char* marker = obj->car->string;
	if(0 == strncmp(marker, "edge", 4)) return tee;
	return nil;
}

/* Agent: (agent id state goals actions) */
struct cell* prim_make_agent(struct cell* args)
{
	if(nil == args || nil == args->cdr) return nil;
	
	struct cell* agent_marker = make_sym("agent");
	struct cell* id = args->car;
	struct cell* state = args->cdr->car;
	struct cell* goals = (nil != args->cdr->cdr) ? args->cdr->cdr->car : nil;
	struct cell* actions = (nil != args->cdr->cdr && nil != args->cdr->cdr->cdr) ? 
						   args->cdr->cdr->cdr->car : nil;
	
	return make_cons(agent_marker,
		   make_cons(id,
			   make_cons(state,
				   make_cons(goals,
					   make_cons(actions, nil)))));
}

/* Check if object is an agent */
struct cell* prim_agent_p(struct cell* args)
{
	if(nil == args) return nil;
	
	struct cell* obj = args->car;
	if(nil == obj || SYM != obj->car->type) return nil;
	
	char* marker = obj->car->string;
	if(0 == strncmp(marker, "agent", 5)) return tee;
	return nil;
}

/* Tensor shape: (tensor-shape dimensions type-sig dof) */
struct cell* prim_make_tensor_shape(struct cell* args)
{
	if(nil == args || nil == args->cdr) return nil;
	
	struct cell* tensor_marker = make_sym("tensor-shape");
	struct cell* dimensions = args->car;
	struct cell* type_sig = args->cdr->car;
	struct cell* dof = (nil != args->cdr->cdr) ? args->cdr->cdr->car : make_int(1);
	
	return make_cons(tensor_marker,
		   make_cons(dimensions,
			   make_cons(type_sig,
				   make_cons(dof, nil))));
}

/* Create a simple hypergraph representation */
struct cell* prim_hypergraph(struct cell* args)
{
	struct cell* hg_marker = make_sym("hypergraph");
	struct cell* nodes = (nil != args) ? args->car : nil;
	struct cell* edges = (nil != args && nil != args->cdr) ? args->cdr->car : nil;
	
	return make_cons(hg_marker,
		   make_cons(nodes,
			   make_cons(edges, nil)));
}

struct cell* prim_list(struct cell* args) {return args;}
struct cell* prim_cons(struct cell* args) { return make_cons(args->car, args->cdr->car); }
struct cell* prim_car(struct cell* args) { return args->car->car; }
struct cell* prim_cdr(struct cell* args) { return args->car->cdr; }

void spinup(struct cell* sym, struct cell* prim)
{
	all_symbols = make_cons(sym, all_symbols);
	top_env = extend(top_env, sym, prim);
}

/*** Initialization ***/
struct cell* intern(char *name);
struct cell* make_prim(void* fun);
struct cell* make_sym(char* name);
void init_sl3()
{
	/* Special symbols */
	nil = make_sym("nil");
	tee = make_sym("#t");
	quote = make_sym("quote");
	s_if = make_sym("if");
	s_cond = make_sym("cond");
	s_lambda = make_sym("lambda");
	s_define = make_sym("define");
	s_setb = make_sym("set!");
	s_begin = make_sym("begin");
	s_let = make_sym("let");

	/* Globals of interest */
	all_symbols = make_cons(nil, nil);
	top_env = extend(nil, nil, nil);

	/* Add Eval Specials */
	spinup(tee, tee);
	spinup(quote, quote);
	spinup(s_if, s_if);
	spinup(s_cond, s_cond);
	spinup(s_lambda, s_lambda);
	spinup(s_define, s_define);
	spinup(s_setb, s_setb);
	spinup(s_begin, s_begin);
	spinup(s_let, s_let);

	/* Add Primitive Specials */
	spinup(make_sym("null?"), make_prim(nullp));
	spinup(make_sym("+"), make_prim(prim_sum));
	spinup(make_sym("-"), make_prim(prim_sub));
	spinup(make_sym("*"), make_prim(prim_prod));
	spinup(make_sym("/"), make_prim(prim_div));
	spinup(make_sym("mod"), make_prim(prim_mod));
	spinup(make_sym("and"), make_prim(prim_and));
	spinup(make_sym("or"), make_prim(prim_or));
	spinup(make_sym("not"), make_prim(prim_not));
	spinup(make_sym(">"), make_prim(prim_numgt));
	spinup(make_sym(">="), make_prim(prim_numge));
	spinup(make_sym("="), make_prim(prim_numeq));
	spinup(make_sym("<="), make_prim(prim_numle));
	spinup(make_sym("<"), make_prim(prim_numlt));
	spinup(make_sym("display"), make_prim(prim_display));
	spinup(make_sym("write"), make_prim(prim_write));
	spinup(make_sym("free_mem"), make_prim(prim_freecell));
	spinup(make_sym("ascii!"), make_prim(prim_ascii));
	spinup(make_sym("list?"), make_prim(prim_listp));
	spinup(make_sym("list"), make_prim(prim_list));
	spinup(make_sym("string=?"), make_prim(prim_stringeq));
	spinup(make_sym("cons"), make_prim(prim_cons));
	spinup(make_sym("car"), make_prim(prim_car));
	spinup(make_sym("cdr"), make_prim(prim_cdr));
	spinup(make_sym("HALT"), make_prim(prim_halt));
	
	/* Cognitive Grammar Primitives - Ghost in the Guile Shell */
	spinup(make_sym("test-cognitive"), make_prim(prim_test_cognitive));
	spinup(make_sym("make-node"), make_prim(prim_make_node));
	spinup(make_sym("node?"), make_prim(prim_node_p));
	spinup(make_sym("node-type"), make_prim(prim_node_type));
	spinup(make_sym("node-value"), make_prim(prim_node_value));
	spinup(make_sym("make-edge"), make_prim(prim_make_edge));
	spinup(make_sym("edge?"), make_prim(prim_edge_p));
	spinup(make_sym("make-agent"), make_prim(prim_make_agent));
	spinup(make_sym("agent?"), make_prim(prim_agent_p));
	spinup(make_sym("make-tensor-shape"), make_prim(prim_make_tensor_shape));
	spinup(make_sym("hypergraph"), make_prim(prim_hypergraph));
}
