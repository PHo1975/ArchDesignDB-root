/**
 * Author: Kathi Started:20.04.2013
 */
package util;
import java.io.Serializable;

/**
 * 
 */
public class ExportContainer implements Serializable {
  public int classID;
  public String[] fields;
  public ExportContainer[] children;
  static final long serialVersionUID = 42752000L;
  
  public ExportContainer(int nclassID,String[] nfields,ExportContainer[] nchildren){  	
  	classID=nclassID;
  	fields=nfields;
  	children=nchildren;  	
  }
  
  @Override public String toString() {
  	return "(ExCo "+classID+" "+java.util.Arrays.toString(fields)+" children:"+java.util.Arrays.toString(children)+")";
  }
}
