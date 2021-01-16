package edu.handong.csee.plt.ast;

public class ReFun extends AST{
	
	String param = "";
	AST body = new AST();
	
	public ReFun(String param, AST body) {
		this.param = param;
		this.body = body;
	}
	
	public String getParam() {
		return param;
	}
	
	public AST getBody() {
		return body;
	}

	public String getASTCode() {
		return "(refun '" + param + " " + body.getASTCode() + ")";
	}
}
