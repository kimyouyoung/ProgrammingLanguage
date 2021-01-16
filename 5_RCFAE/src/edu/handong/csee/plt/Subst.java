package edu.handong.csee.plt;

import edu.handong.csee.plt.ast.*;

public class Subst {
	
	public AST subst(AST ast, String name, AST num) {
				
		if(ast instanceof Num) {
			return ast;
		}
		
		if(ast instanceof Add) {
			Add add = (Add)ast;
			return new Add((subst(add.getLhs(), name, num)), (subst(add.getRhs(), name, num)));
		}
		
		if(ast instanceof Sub) {
			Sub sub = (Sub)ast;
			return new Sub((subst(sub.getLhs(), name, num)), (subst(sub.getRhs(), name, num)));
		}
		
		if(ast instanceof With) {
			With with = (With)ast;
			if(with.getName().equals(name))
				return new With(with.getName(), subst(with.getValue(), name, num), with.getBody());
			else
				return new With(with.getName(), subst(with.getValue(), name, num), subst(with.getBody(), name, num));
		}
		
		if(ast instanceof Id) {
			Id id = (Id)ast;
			
			if(id.getName().equals(name)) 
				return num;
				
			else
				return ast;
		}
		
		if(ast instanceof App) {
			App app = (App)ast;
			return new App((subst(app.getFtn(), name, num)), (subst(app.getArg(), name, num)));
		}
		
		if(ast instanceof Fun) {
			Fun fun = (Fun)ast;
			if(fun.getParam().equals(name))
				return ast;
			else
				return new Fun(fun.getParam(), (subst(fun.getBody(), name, num)));
		}
		
		return ast;
	}
}
