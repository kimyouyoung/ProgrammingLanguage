package edu.handong.csee.plt;

import java.util.Vector;

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
			Strict strict = new Strict();
			return strict.strict(lookup.lookUp(id.getName(), ds));
		}
		
		if(ast instanceof Fun) {
			Fun fun = (Fun)ast;
			return new ClosureV(fun.getParam(), fun.getBody(), ds);
		}
		
		if(ast instanceof If0) {
			If0 if0 = (If0)ast;
			
			if(numZero(interp(if0.getTest(), ds)))
				return interp(if0.getThen(), ds);
			
			else
				return interp(if0.getElse(), ds);
			
		}
		
		if(ast instanceof App) {
			App app = (App)ast;
			Strict strict = new Strict();
			
			ClosureV fval = (ClosureV)strict.strict(interp(app.getFtn(), ds));
			exprV aval = new exprV(app.getArg(), ds, new Vector<Value>());
	
			return interp(fval.getBody(), new aSub(fval.getParam(), aval, fval.getDefrdSub()));
		}
		
		if(ast instanceof Rec) {
			Rec rec = (Rec)ast;
			Vector<Value> v_holder = new Vector<Value>();
			aRecSub new_ds = new aRecSub(rec.getName(), v_holder, ds);
			
			v_holder.add(interp(rec.getExp(), new_ds));
			
			return interp(rec.getFcall(), new_ds);
		}
		
		return null;
	}
	
	public boolean numZero(Value v) {
		
		String num = v.getValueCode();
		num = num.replaceAll("[^0-9]", "");
		
		if(num.equals("0"))
			return true;
		
		return false;
	}
}
