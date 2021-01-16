package edu.handong.csee.plt;

import edu.handong.csee.plt.val.*;

public class Strict {
	
public Value strict(Value val) {
		
		if(val instanceof exprV) {
			exprV exprv = (exprV)val;
			Interpreter interp = new Interpreter();
			
			if(exprv.getValue().isEmpty()) {
				Value v = strict(interp.interp(exprv.getExpr(), exprv.getDefrdSub()));
				exprv.getValue().add(v);
				return v;
			}
			else
				return exprv.getValue().lastElement();
		}
		
		return val;
	}

}
