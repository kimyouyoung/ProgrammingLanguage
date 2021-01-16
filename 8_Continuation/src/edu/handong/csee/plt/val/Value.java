package edu.handong.csee.plt.val;

public class Value {
	
	public String getValueCode() {
		
		String valueCode="";
		if(this instanceof numV)
			valueCode = ((numV)this).getValueCode();
		
		if(this instanceof ClosureV)
			valueCode = ((ClosureV)this).getValueCode();
		
		if(this instanceof refclosV)
			valueCode = ((refclosV)this).getValueCode();
		
		if(this instanceof exprV)
			valueCode = ((exprV)this).getValueCode();
		
		if(this instanceof boxV)
			valueCode = ((boxV)this).getValueCode();
		

		return valueCode;
	}
}
