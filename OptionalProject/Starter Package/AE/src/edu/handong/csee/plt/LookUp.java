package edu.handong.csee.plt;

import edu.handong.csee.plt.defsub.*;

public class LookUp {
	
	public int lookUp(String name, DefrdSub ds) {
		
		if(ds instanceof mtSub) {
			System.out.println("Free identifier");
			System.exit(0);
		}
		
		if(ds instanceof aSub) {
			aSub asub = (aSub)ds;
			
			if(asub.getName().equals(name))
				return asub.getAddress();
			
			else
				return lookUp(name, asub.getDefrdSub());
		}
		
		return -1;
	}
	
}
