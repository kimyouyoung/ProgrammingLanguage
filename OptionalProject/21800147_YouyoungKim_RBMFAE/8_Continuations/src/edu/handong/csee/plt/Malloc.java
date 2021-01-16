package edu.handong.csee.plt;

import edu.handong.csee.plt.store.*;

public class Malloc {
	
	public int malloc(Store sto) {
		return maxAddress(sto) + 1;
	}
	
	public int maxAddress(Store sto) {
		
		if(sto instanceof mtSto)
			return 0;
		
		if(sto instanceof aSto) {
			aSto asto = (aSto)sto;
			
			int max = Math.max(asto.getAddress(), maxAddress(asto.getStore()));
			return max;
		}
		
		return -1;
		
	}
	
}
