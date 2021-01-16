package edu.handong.csee.plt.ast;

public class Sub extends AST{
	AST lhs = new AST();
	AST rhs = new AST();
	
	public Sub(AST lhs, AST rhs) {
		this.lhs = lhs;
		this.rhs = rhs;
	}
	
	public AST getLhs() {
		return lhs;
	}

	public AST getRhs() {
		return rhs;
	}

	public String getASTCode() {
		return "(sub " + lhs.getASTCode() + " " + rhs.getASTCode() + ")";
	}
}
