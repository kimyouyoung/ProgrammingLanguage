package edu.handong.csee.plt.store;

import edu.handong.csee.plt.val.*;

public class aSto extends Store{
	
	int address = 0;
	Value val = new Value();
	Store store = new Store();
	
	public aSto(int address, Value val, Store store) {
		this.address = address;
		this.val = val;
		this.store = store;
	}
	
	public int getAddress() {
		return address;
	}
	
	public Value getValue() {
		return val;
	}
	
	public Store getStore() {
		return store;
	}

	public String getStoreCode() {
		return "(aSto " + address + " " + val.getValueCode() + " " + store.getStoreCode() + ")";
	}
	
}
