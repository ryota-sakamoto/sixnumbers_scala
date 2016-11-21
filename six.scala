import scala.io.StdIn._;

object six{
    private var target = 0;

    def main(args : Array[String]){
        var s = Array.ofDim[Int](6);
        val is = Array.ofDim[(Int,String)](500000);
        for(i<- 0 until 6){
            printf("数字を入力してください(%d/6):",i+1);
            s(i)=readLine().toInt;
            is(i)=(s(i),i.toString);
        }
        printf("6 つの数字を四則演算して導き出す数字を入力してください:");
        target = readLine().toInt;

        val answer = aa(is,s);
        q(answer,s);
    }

    def aa(is:Array[(Int,String)],s:Array[Int]):String={
        var r = 0;
        var n = 0;
        for(i<- 0 until 6;j<- i until 6){
            if(i!=j){
                is(n) = (m(s(i),s(j),"+"),i.toString+"+"+j.toString);n+=1;
                is(n) = (m(s(i),s(j),"*"),i.toString+"*"+j.toString);n+=1;
                if({r = m(s(i),s(j),"/");r!=0}){
                    is(n) = (r,i.toString+"/"+j.toString);n+=1;
                }
                if({r = m(s(i),s(j),"-");r!=0}){
                    is(n) = (m(s(i),s(j),"-"),i.toString+"-"+j.toString);n+=1;
                }
            }
        }

        for(k<- 0 until 4;i<- 0 until n){
            if(is(i)!=null){
                for(j<- 0 until 6){
                    if(!is(i)._2.contains(j.toString)){
                        is(n) = (m(is(i)._1,s(j),"+"),is(i)._2+"+"+j.toString);n+=1;
                        is(n) = (m(is(i)._1,s(j),"*"),is(i)._2+"*"+j.toString);n+=1;
                        if({r = m(is(i)._1,s(j),"/");r!=0}){
                            is(n) = (r,is(i)._2+"/"+j.toString);n+=1;
                        }
                        if({r = m(is(i)._1,s(j),"-");r!=0}){
                            is(n) = (r,is(i)._2+"-"+j.toString);n+=1;
                        }
                    }
                }
            }
        }
        for(i<- 0 until is.length){
            if(is(i)._1==target){
                return is(i)._2;
            }
        }
        "";
    }

    def m(a:Int,b:Int,k:String):Int={
        var r = k match {
            case "+" => a+b;
            case "-" => if(a>b){
                    a-b;
                }else if(b>a){
                    b-a;
                }else{
                    0;
                }
            case "*" => a*b;
            case "/" => if(a%b==0){
                    a/b;
                }else if(b%a==0){
                    b/a;
                }else{
                    0;
                }
        }
        r;
    }

    def q(r:String,number:Array[Int]){
        val s = """\d""";
        var a = 0;
        var b = 0;
        var c = 0;
        for(i<- 0 until r.length){
            if(r(i).toString.matches(s)){
                if(i==0){
                    a=number(r(i).toString.toInt);
                }else{
                    b=number(r(i).toString.toInt);
                    if(a<b || a>b){
                        c=a;
                        a=b;
                        b=c;
                    }
                    c=m(a,b,r(i-1).toString);
                    printf("%d %s %d = %d\n",a,r(i-1),b,c);
                    a=c;
                }
            }
        }
    }
}