package edu.handong.csee.plt.val;


import java.util.Vector;

import edu.handong.csee.plt.ast.*;
import edu.handong.csee.plt.defsub.*;

public class exprV extends Value{
	
	AST expr = new AST();
	DefrdSub ds = new DefrdSub();
	Vector<Value> value = new Vector<Value>();
	
	public exprV(AST expr, DefrdSub ds, Vector<Value> value) {
		this.expr = expr;
		this.ds = ds;
		this.value = value;
	}
	
	public AST getExpr() {
		return expr;
	}
	
	public DefrdSub getDefrdSub() {
		return ds;
	}
	
	public Vector<Value> getValue() {
		return value;
	}

	public String getValueCode() {
		String str = "(exprV " + expr.getASTCode() + " " + ds.getDefrdSubCode();
		
		for(Value v: value)
			str = str + " " + v.getValueCode();
		
		return  str + ")";
	}
}
