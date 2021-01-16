package edu.handong.csee.plt.defsub;

import edu.handong.csee.plt.val.*;

public class aSub extends DefrdSub{
	
	String name = "";
	Value val = new Value();
	DefrdSub ds = new DefrdSub();
	
	public aSub(String name, Value val, DefrdSub ds) {
		this.name = name;
		this.val = val;
		this.ds = ds;
	}
	
	public String getName() {
		return name;
	}
	
	public Value getVal() {
		return val;
	}
	
	public DefrdSub getDefrdSub() {
		return ds;
	}

	public String getDefrdSubCode() {
		return "(aSub '" + name + " " + val.getValueCode() + " " +  ds.getDefrdSubCode() + ")";
	}
}
