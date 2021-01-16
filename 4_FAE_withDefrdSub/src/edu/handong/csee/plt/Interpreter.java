package edu.handong.csee.plt;

import edu.handong.csee.plt.ast.*;
import edu.handong.csee.plt.defsub.*;
import edu.handong.csee.plt.val.*;

public class Interpreter {

	public Value interp(AST ast, DefrdSub ds) {
		
		if(ast instanceof Num) {
			return new numV(((Num) ast).getStrNum());
		}
		
		if(ast instanceof Add) {
			Add add = (Add)ast;
			NumOp numop = new NumOp();
			String num = numop.NumOperator("+", interp(add.getLhs(), ds).getValueCode(), interp(add.getRhs(), ds).getValueCode());
			return new numV(num);
		}
		
		if(ast instanceof Sub) {
			Sub sub = (Sub)ast;
			NumOp numop = new NumOp();
			String num = numop.NumOperator("-", interp(sub.getLhs(), ds).getValueCode(), interp(sub.getRhs(), ds).getValueCode());
			return new numV(num);
		}
		
		if(ast instanceof Id) {
			Id id = (Id)ast;
			LookUp lookup = new LookUp();
			return lookup.lookUp(id.getName(), ds);
		}
		
		if(ast instanceof Fun) {
			Fun fun = (Fun)ast;
			return new ClosureV(fun.getParam(), fun.getBody(), ds);
		}
		
		if(ast instanceof App) {
			App app = (App)ast;
			ClosureV fval = (ClosureV)interp(app.getFtn(), ds);
			Value aval = (Value)interp(app.getArg(), ds);
	
			return interp(fval.getBody(), new aSub(fval.getParam(), aval, fval.getDefrdSub()));
		}
		
		return null;
	}
}
