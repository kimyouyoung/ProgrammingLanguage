package edu.handong.csee.plt;

import edu.handong.csee.plt.ast.*;

public class Interpreter {

	public String interp(AST ast) {
		
		if(ast instanceof Num) {
			return ((Num)ast).getStrNum();
		}
		
		if(ast instanceof Add) {
			Add add = (Add)ast;
			return "" + (Integer.parseInt(interp(add.getLhs())) + Integer.parseInt(interp(add.getRhs())));
		}
		
		if(ast instanceof Sub) {
			Sub sub = (Sub)ast;
			return "" + (Integer.parseInt(interp(sub.getLhs())) - Integer.parseInt(interp(sub.getRhs())));
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
		
		return null;
	}
}
