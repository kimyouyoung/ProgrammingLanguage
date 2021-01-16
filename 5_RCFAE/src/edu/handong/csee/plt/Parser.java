package edu.handong.csee.plt;

import java.util.ArrayList;

import edu.handong.csee.plt.ast.*;

public class Parser {

	AST parse(String exampleCode) {
		ArrayList<String> subExpressions = splitExpressionAsSubExpressions(exampleCode);
		
		// num
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
		
		// with
		if(subExpressions.get(0).equals("with")) {
			String name = subExpressions.get(1).substring(1, subExpressions.get(1).indexOf(" "));
			String value = subExpressions.get(1).substring(subExpressions.get(1).indexOf(" ")+1, subExpressions.get(1).length() - 1);
			
			return new App(new Fun(name,parse(subExpressions.get(2))), parse(value));
		}
		
		// fun
		if(subExpressions.get(0).equals("fun")) {
			String param = subExpressions.get(1).substring(1, subExpressions.get(1).indexOf("}"));
			return new Fun(param, parse(subExpressions.get(2)));
		}
		
		// app
		if(subExpressions.size() == 2) {
			
			return new App(parse(subExpressions.get(0)), parse(subExpressions.get(1)));
		}
		
		// if0
		if(subExpressions.get(0).equals("if0")) {
			
			return new If0(parse(subExpressions.get(1)), parse(subExpressions.get(2)), parse(subExpressions.get(3)));
		}
		
		// rec
		if(subExpressions.get(0).equals("rec")) {
			String rfn = subExpressions.get(1).substring(1, subExpressions.get(1).indexOf(" "));
			String ne = subExpressions.get(1).substring(subExpressions.get(1).indexOf(" ")+1, subExpressions.get(1).length() - 1);
			
			return new Rec(rfn, parse(ne), parse(subExpressions.get(2)));
		}
		
		// id
		if(subExpressions.size() == 1 && !isNumeric(subExpressions.get(0))) {
			
			return new Id(subExpressions.get(0));
		}
	
		System.out.println("Error! bad syntax:(");
		System.exit(0);
		
		return null;
	}

	private ArrayList<String> splitExpressionAsSubExpressions(String exampleCode) {

		// deal with brackets first.
		if((exampleCode.startsWith("{") && !exampleCode.endsWith("}"))
				|| (!exampleCode.startsWith("{") && exampleCode.endsWith("}"))) {
			System.out.println("Syntax error");
			System.exit(0);
		}

		if(exampleCode.startsWith("{")) {
			exampleCode = exampleCode.substring(1, exampleCode.length()-1);
	
			if(exampleCode.startsWith("{") && exampleCode.charAt(exampleCode.length()-1) == '}') {
				ArrayList<String> arr = new ArrayList<String>();
				String str = "{";
				int count = 1;
				for(int i = 1; i < exampleCode.length(); i++) {
					if(count == 0 && exampleCode.charAt(i) == ' ')
						continue;
					
					str = str + exampleCode.charAt(i);
					
					if(exampleCode.charAt(i) == '{')
						count++;
					else if(exampleCode.charAt(i) == '}')
						count--;
					
					if(count == 0) {
						arr.add(str);
						str = "";
					}
				}
				
				return arr;
			}
		}

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
					strBuffer = "";
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
