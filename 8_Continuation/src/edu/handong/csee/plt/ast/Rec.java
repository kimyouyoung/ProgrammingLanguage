package edu.handong.csee.plt.ast;

public class Rec extends AST{
	
	String name = "";
	AST exp = new AST();
	AST f_call = new AST();
	
	public Rec(String name, AST exp, AST f_call) {
		this.name = name;
		this.exp = exp;
		this.f_call = f_call;
	}
	
	public String getName() {
		return name;
	}

	public AST getExp() {
		return exp;
	}
	
	public AST getFcall() {
		return f_call;
	}

	public String getASTCode() {
		return "(rec '" + name + " " + exp.getASTCode() + " " + f_call.getASTCode() + ")";
	}
}
