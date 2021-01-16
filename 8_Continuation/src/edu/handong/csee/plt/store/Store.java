package edu.handong.csee.plt.store;


public class Store {
	
	public String getStoreCode() {
		
		String storeCode="";
		
		if(this instanceof mtSto)
			storeCode = ((mtSto)this).getStoreCode();
		
		if(this instanceof aSto)
			storeCode = ((aSto)this).getStoreCode();
		

		return storeCode;
	}
}
