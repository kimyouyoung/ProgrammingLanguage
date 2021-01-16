package edu.handong.csee.plt;

import edu.handong.csee.plt.store.*;
import edu.handong.csee.plt.val.*;

public class StoreLookUp {
	
	public Value storeLookUp(int address, Store sto) {
		
		if(sto instanceof mtSto) {
			System.out.println("No value at address");
			System.exit(0);
		}
		
		if(sto instanceof aSto) {
			aSto asto = (aSto)sto;
			
			if(asto.getAddress() == address) {
				return asto.getValue();
			}
			
			else
				return storeLookUp(address, asto.getStore());
		}
		
		return null;
	}
}
