package edu.handong.csee.plt;

import java.util.ArrayList;

import edu.handong.csee.plt.ast.*;

public class Parser {

	AST parse(String exampleCode) {
		ArrayList<String> subExpressions = splitExpressionAsSubExpressions(exampleCode);
		
		if(subExpressions.size() == 1 && isNumeric(subExpressions.get(0))) {

			return new Num(subExpressions.get(0));
		}

		// add
		if(subExpressions.get(0).equals("+")) {
			
			return new Add(parse(subExpressions.get(1)),parse(subExpressions.get(2)));
		}
		
		// sub
		if(subExpressions.get(0).equals("-")) {
			
			return new Sub(parse(subExpressions.get(1)),parse(subExpressions.get(2)));
		}

		
		return null;
	}

	private ArrayList<String> splitExpressionAsSubExpressions(String exampleCode) {

		// deal with brackets first.
		if((exampleCode.startsWith("{") && !exampleCode.endsWith("}"))
				|| (!exampleCode.startsWith("{") && exampleCode.endsWith("}"))) {
			System.out.println("Syntax error");
			System.exit(0);
		}

		if(exampleCode.startsWith("{"))
			exampleCode = exampleCode.substring(1, exampleCode.length()-1);


		return getSubExpressions(exampleCode);
	}



	/**
	 * This method return a list of sub-expression from the given expression.
	 * For example, {+ 3 {+ 3 4}  -> +, 2, {+ 3 4}
	 * TODO JC was sleepy while implementing this method...it has complex logic and might be buggy...
	 * You can do better or find an external library.
	 * @param exampleCode
	 * @return list of sub expressions 
	 */
	private ArrayList<String> getSubExpressions(String exampleCode) {

		ArrayList<String> sexpressions = new ArrayList<String>();
		int openingParenthesisCount = 0;
		String strBuffer = "";
		for(int i=0; i < exampleCode.length()  ;i++) {
			if(i==0 || (i==0 && exampleCode.charAt(i) == '{')) {
				strBuffer = strBuffer + exampleCode.charAt(i);
				continue;
			} else if(exampleCode.charAt(i)==' ' && openingParenthesisCount==0){
				// buffer is ready to be a subexpression
				if(!strBuffer.isEmpty()) {
					sexpressions.add(strBuffer);
					strBuffer = ""; // Ready to start a new buffer
				}
				continue;
			} else {
				if(exampleCode.charAt(i)=='{' && openingParenthesisCount==0){
					openingParenthesisCount++;
					// Ready to start a new buffer
					strBuffer = "" + exampleCode.charAt(i);
					continue;
				} else if(exampleCode.charAt(i)=='{') {
					openingParenthesisCount++;
					strBuffer = strBuffer + exampleCode.charAt(i);
					continue;
				} else if(exampleCode.charAt(i)=='}' && openingParenthesisCount>0) {
					openingParenthesisCount--;
					strBuffer = strBuffer + exampleCode.charAt(i);
					continue;
				} else if(exampleCode.charAt(i)=='}') {
					// buffer is ready to be a subexpression
					sexpressions.add(strBuffer);
					continue;
				}
			}
			strBuffer = strBuffer + exampleCode.charAt(i);
		}
		
		sexpressions.add(strBuffer);

		return sexpressions;
	}

	public static boolean isNumeric(String str)
	{
		return str.matches("-?\\d+(\\.\\d+)?");  //match a number with optional '-' and decimal.
	}

}
