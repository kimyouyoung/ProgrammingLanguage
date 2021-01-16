package edu.handong.csee.plt.defsub;

import java.util.Vector;

import edu.handong.csee.plt.val.*;

public class aRecSub extends DefrdSub{
	
	String name = "";
	Vector<Value> val = new Vector<Value>();
	DefrdSub ds = new DefrdSub();
	
	public aRecSub(String name, Vector<Value> val, DefrdSub ds) {
		this.name = name;
		this.val = val;
		this.ds = ds;
	}
	
	public String getName() {
		return name;
	}
	
	public Vector<Value> getVal() {
		return val;
	}
	
	public DefrdSub getDefrdSub() {
		return ds;
	}

	public String getDefrdSubCode() {
		String str = "(aRecSub '" + name;
		
		for(Value v : val)
			str = str + " " + v.getValueCode();
		
		return str + " " +  ds.getDefrdSubCode() + ")";
	}
	
}
