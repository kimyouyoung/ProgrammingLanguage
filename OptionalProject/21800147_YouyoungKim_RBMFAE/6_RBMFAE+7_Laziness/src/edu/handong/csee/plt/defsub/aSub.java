package edu.handong.csee.plt.defsub;


public class aSub extends DefrdSub{
	
	String name = "";
	int address = 0;
	DefrdSub ds = new DefrdSub();
	
	public aSub(String name, int address, DefrdSub ds) {
		this.name = name;
		this.address = address;
		this.ds = ds;
	}
	
	public String getName() {
		return name;
	}
	
	public int getAddress() {
		return address;
	}
	
	public DefrdSub getDefrdSub() {
		return ds;
	}

	public String getDefrdSubCode() {
		return "(aSub '" + name + " " + address + " " +  ds.getDefrdSubCode() + ")";
	}
}
