import{a as q}from"./chunk-MF5TUIBL.js";import{a as j}from"./chunk-5XJO6U7A.js";import{a as H}from"./chunk-E4RBDSW5.js";import"./chunk-NKCQAART.js";import"./chunk-WNVR7S66.js";import"./chunk-ZP5QXAVO.js";import{k as V,l as Z}from"./chunk-47P5NBBB.js";import"./chunk-XHGORZV2.js";import{Da as w,Ga as U,O as G,S as O,T as M,U as P,V as R,W as I,X as L,Y as W,Z as N,h as i,j as h,ka as B,v as F}from"./chunk-KRX7QNR4.js";import"./chunk-UY4K4Z6W.js";import"./chunk-TZMIYTTY.js";import"./chunk-SA2EGKJT.js";import"./chunk-WYMAA4MH.js";import"./chunk-SISR4MA5.js";import"./chunk-24JW6VB3.js";import"./chunk-MGYUK2XN.js";var J=F.pie,y={sections:new Map,showData:!1,config:J},v=y.sections,T=y.showData,ne=structuredClone(J),oe=i(()=>structuredClone(ne),"getConfig"),se=i(()=>{v=new Map,T=y.showData,O()},"clear"),le=i(({label:e,value:a})=>{v.has(e)||(v.set(e,a),h.debug(`added new section: ${e}, with value: ${a}`))},"addSection"),ce=i(()=>v,"getSections"),pe=i(e=>{T=e},"setShowData"),de=i(()=>T,"getShowData"),K={getConfig:oe,clear:se,setDiagramTitle:L,getDiagramTitle:W,setAccTitle:M,getAccTitle:P,setAccDescription:R,getAccDescription:I,addSection:le,getSections:ce,setShowData:pe,getShowData:de},ge=i((e,a)=>{q(e,a),a.setShowData(e.showData),e.sections.map(a.addSection)},"populateDb"),fe={parse:i(async e=>{let a=await H("pie",e);h.debug(a),ge(a,K)},"parse")},ue=i(e=>`
  .pieCircle{
    stroke: ${e.pieStrokeColor};
    stroke-width : ${e.pieStrokeWidth};
    opacity : ${e.pieOpacity};
  }
  .pieOuterCircle{
    stroke: ${e.pieOuterStrokeColor};
    stroke-width: ${e.pieOuterStrokeWidth};
    fill: none;
  }
  .pieTitleText {
    text-anchor: middle;
    font-size: ${e.pieTitleTextSize};
    fill: ${e.pieTitleTextColor};
    font-family: ${e.fontFamily};
  }
  .slice {
    font-family: ${e.fontFamily};
    fill: ${e.pieSectionTextColor};
    font-size:${e.pieSectionTextSize};
    // fill: white;
  }
  .legend text {
    fill: ${e.pieLegendTextColor};
    font-family: ${e.fontFamily};
    font-size: ${e.pieLegendTextSize};
  }
`,"getStyles"),me=ue,he=i(e=>{let a=[...e.entries()].map(n=>({label:n[0],value:n[1]})).sort((n,l)=>l.value-n.value);return U().value(n=>n.value)(a)},"createPieArcs"),ve=i((e,a,Q,n)=>{h.debug(`rendering pie chart
`+e);let l=n.db,$=N(),A=Z(l.getConfig(),$.pie),_=40,o=18,g=4,c=450,S=c,x=j(a),p=x.append("g");p.attr("transform","translate("+S/2+","+c/2+")");let{themeVariables:r}=$,[f]=V(r.pieOuterStrokeWidth);f!=null||(f=2);let E=A.textPosition,u=Math.min(S,c)/2-_,X=w().innerRadius(0).outerRadius(u),Y=w().innerRadius(u*E).outerRadius(u*E);p.append("circle").attr("cx",0).attr("cy",0).attr("r",u+f/2).attr("class","pieOuterCircle");let b=l.getSections(),C=he(b),ee=[r.pie1,r.pie2,r.pie3,r.pie4,r.pie5,r.pie6,r.pie7,r.pie8,r.pie9,r.pie10,r.pie11,r.pie12],d=B(ee);p.selectAll("mySlices").data(C).enter().append("path").attr("d",X).attr("fill",t=>d(t.data.label)).attr("class","pieCircle");let k=0;b.forEach(t=>{k+=t}),p.selectAll("mySlices").data(C).enter().append("text").text(t=>(t.data.value/k*100).toFixed(0)+"%").attr("transform",t=>"translate("+Y.centroid(t)+")").style("text-anchor","middle").attr("class","slice"),p.append("text").text(l.getDiagramTitle()).attr("x",0).attr("y",-(c-50)/2).attr("class","pieTitleText");let D=p.selectAll(".legend").data(d.domain()).enter().append("g").attr("class","legend").attr("transform",(t,s)=>{let m=o+g,ae=m*d.domain().length/2,re=12*o,ie=s*m-ae;return"translate("+re+","+ie+")"});D.append("rect").attr("width",o).attr("height",o).style("fill",d).style("stroke",d),D.data(C).append("text").attr("x",o+g).attr("y",o-g).text(t=>{let{label:s,value:m}=t.data;return l.getShowData()?`${s} [${m}]`:s});let te=Math.max(...D.selectAll("text").nodes().map(t=>{var s;return(s=t==null?void 0:t.getBoundingClientRect().width)!=null?s:0})),z=S+_+o+g+te;x.attr("viewBox",`0 0 ${z} ${c}`),G(x,c,z,A.useMaxWidth)},"draw"),Se={draw:ve},$e={parser:fe,db:K,renderer:Se,styles:me};export{$e as diagram};
