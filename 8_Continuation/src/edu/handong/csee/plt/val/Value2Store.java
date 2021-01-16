package edu.handong.csee.plt.val;

import edu.handong.csee.plt.store.Store;

public class Value2Store {
	
	Value val1 = new Value();
	Value val2 = new Value();
	Store sto = new Store();
	
	public Value2Store(Value val1, Value val2,  Store sto){
		this.val1 = val1;
		this.val2 = val2;
		this.sto = sto;
	}
	
	public Value getValue1() {
		return val1;
	}
	
	public Value getValue2() {
		return val2;
	}
	
	public Store getStore() {
		return sto;
	}
}
