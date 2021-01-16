package edu.handong.csee.plt;

import edu.handong.csee.plt.ast.*;

public class Interpreter {

	public AST interp(AST ast) {
		
		if(ast instanceof Num) {
			return ((Num)ast);
		}
		
		if(ast instanceof Add) {
			Add add = (Add)ast;
			NumOp numop = new NumOp();
			String num = numop.NumOperator("+", interp(add.getLhs()).getASTCode(), interp(add.getRhs()).getASTCode());
			return new Num(num);
		}
		
		if(ast instanceof Sub) {
			Sub sub = (Sub)ast;
			NumOp numop = new NumOp();
			String num = numop.NumOperator("-", interp(sub.getLhs()).getASTCode(), interp(sub.getRhs()).getASTCode());
			return new Num(num);
		}
		
		if(ast instanceof With) {
			With with = (With)ast;
			Subst subst = new Subst();
			return interp(subst.subst(with.getBody(), with.getName(), interp(with.getValue())));
		}
		
		if(ast instanceof Id) {
			System.out.println("Free identifier");
			System.exit(0);
		}
		
		if(ast instanceof Fun) {
			return ast;
		}
		
		if(ast instanceof App) {
			App app = (App)ast;
			Fun ftn = (Fun)interp(app.getFtn());
			Subst subst = new Subst();
			return interp((subst.subst(ftn.getBody(), ftn.getParam(), interp(app.getArg()))));
		}
		
		return null;
	}
}
