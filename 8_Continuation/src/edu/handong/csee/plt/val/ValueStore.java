package edu.handong.csee.plt.val;

import edu.handong.csee.plt.store.*;

public class ValueStore{
	
	Value val = new Value();
	Store sto = new Store();
	
	public ValueStore(Value val, Store sto){
		this.val = val;
		this.sto = sto;
	}
	
	public Value getValue() {
		return val;
	}
	
	public Store getStore() {
		return sto;
	}
	
	public String getValueStoreCode() {
		return "(v*s " + val.getValueCode() + " " + sto.getStoreCode() +")";
	}	

}
