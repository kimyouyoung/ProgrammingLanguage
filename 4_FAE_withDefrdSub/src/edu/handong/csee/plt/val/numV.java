package edu.handong.csee.plt.val;

public class numV extends Value{
	
	String num = "0";
	
	public numV(String num){
		this.num = num;
	}
	
	public String getStrNum() {
		return num;
	}
	
	public String getValueCode() {
		return "(num " + num +")";
	}
}
