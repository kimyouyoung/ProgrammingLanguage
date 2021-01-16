package edu.handong.csee.plt;

import edu.handong.csee.plt.defsub.*;
import edu.handong.csee.plt.val.Value;

public class LookUp {
	
	public Value lookUp(String name, DefrdSub ds) {
		
		if(ds instanceof mtSub) {
			System.out.println("Free identifier");
			System.exit(0);
		}
		
		if(ds instanceof aSub) {
			aSub asub = (aSub)ds;
			
			if(asub.getName().equals(name))
				return asub.getVal();
			
			else
				return lookUp(name, asub.getDefrdSub());
		}
		
		return null;
	}
	
}
