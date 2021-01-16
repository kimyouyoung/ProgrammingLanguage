package edu.handong.csee.plt;

import edu.handong.csee.plt.ast.*;
import edu.handong.csee.plt.defsub.*;
import edu.handong.csee.plt.store.*;
import edu.handong.csee.plt.val.*;

public class interTwo {
	
	public Value2Store interptwo(AST exp1, AST exp2, DefrdSub ds, Store st) {
		
		Strict strict = new Strict();
		Interpreter interp = new Interpreter();
		ValueStore lvs = strict.strict(interp.interp(exp1, ds, st));
		ValueStore rvs = strict.strict(interp.interp(exp2, ds, lvs.getStore()));
		
		return new Value2Store(lvs.getValue(), rvs.getValue(), rvs.getStore());
		
	}
	
}
