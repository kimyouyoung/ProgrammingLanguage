package edu.handong.csee.plt.val;

public class boxV extends Value{
	
	int address = 0;
	
	public boxV(int address){
		this.address = address;
	}
	
	public int getAddress() {
		return address;
	}
	
	public String getValueCode() {
		return "(boxV " + address +")";
	}	
}
