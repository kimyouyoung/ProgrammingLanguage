package edu.handong.csee.plt;

import edu.handong.csee.plt.val.*;

public class Strict {
	
public ValueStore strict(ValueStore valSto) {
		
		Value val = valSto.getValue();
		
		if(val instanceof exprV) {
			
			exprV exprv = (exprV)val;
			Interpreter interp = new Interpreter();
			
			if(exprv.getValue().isEmpty() || exprv.getValue().firstElement().getValueCode().equals("'#&#f")) {
				
				ValueStore v = strict(interp.interp(exprv.getExpr(), exprv.getDefrdSub(), valSto.getStore()));
				Value value = v.getValue();
				
				if(exprv.getValue().firstElement().getValueCode().equals("'#&#f")) {
					exprv.getValue().removeAllElements();
				}
				
				exprv.getValue().add(value);
				
				return v;
			}
			else
				return new ValueStore(exprv.getValue().lastElement(), valSto.getStore());
		}
		
		return valSto;
	}

}
