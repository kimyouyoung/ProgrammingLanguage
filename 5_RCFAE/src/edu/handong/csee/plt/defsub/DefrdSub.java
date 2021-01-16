package edu.handong.csee.plt.defsub;

public class DefrdSub {
	
	public String getDefrdSubCode() {
		
		String defrdSubCode="";
		
		if(this instanceof mtSub)
			defrdSubCode = ((mtSub)this).getDefrdSubCode();
		
		if(this instanceof aSub)
			defrdSubCode = ((aSub)this).getDefrdSubCode();

		if(this instanceof aRecSub)
			defrdSubCode = ((aRecSub)this).getDefrdSubCode();
		

		return defrdSubCode;
	}

}
