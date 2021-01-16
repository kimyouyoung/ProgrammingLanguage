package edu.handong.csee.plt.val;

public class Value {
	
	public String getValueCode() {
		
		String valueCode="";
		if(this instanceof numV)
			valueCode = ((numV)this).getValueCode();
		
		if(this instanceof ClosureV)
			valueCode = ((ClosureV)this).getValueCode();
		

		return valueCode;
	}
}
