package edu.handong.csee.plt;

public class NumOp {
	
	public String NumOperator(String op, String x, String y) {
		
		String result = "";
		
		x = x.replaceAll("[^0-9]", "");
		
		y = y.replaceAll("[^0-9]", "");
		
		if(op.equals("+"))
			result = "" + (Integer.parseInt(x) + Integer.parseInt(y));
		else if(op.equals("-"))
			result = "" + (Integer.parseInt(x) - Integer.parseInt(y));
		
		return result;
	}
	
}
