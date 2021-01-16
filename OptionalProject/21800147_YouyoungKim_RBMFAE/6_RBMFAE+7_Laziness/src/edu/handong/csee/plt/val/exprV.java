package edu.handong.csee.plt.val;


import java.util.Vector;

import edu.handong.csee.plt.ast.*;
import edu.handong.csee.plt.defsub.*;
import edu.handong.csee.plt.store.*;

public class exprV extends Value{
	
	AST expr = new AST();
	DefrdSub ds = new DefrdSub();
	Store sto = new Store();
	Vector<Value> value = new Vector<Value>();
	
	public exprV(AST expr, DefrdSub ds,  Store sto, Vector<Value> value) {
		this.expr = expr;
		this.ds = ds;
		this.sto = sto;
		this.value = value;
	}
	
	public AST getExpr() {
		return expr;
	}
	
	public DefrdSub getDefrdSub() {
		return ds;
	}
	
	public Store getStore() {
		return sto;
	}
	
	public Vector<Value> getValue() {
		return value;
	}

	public String getValueCode() {
		String str = "(exprV " + expr.getASTCode() + " " + ds.getDefrdSubCode() + " " + sto.getStoreCode();
		boolean first = false;
		
		if(value.size() == 1 && value.firstElement() instanceof First) {
			First f = (First)value.firstElement();
			if(f.getValueCode().equals("'#&#f"))
				first = true;
		}
		if(!first)
			str = str + "(box";
		
		for(Value v: value) 
			str = str + " " + v.getValueCode();
		
		if(!first)
			str = str + ")";
		
		return  str + ")";
	}
}
