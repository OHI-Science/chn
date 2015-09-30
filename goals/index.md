---
layout: article
title: "Goals"
excerpt: "OHI goals for China"
share: false
ads: false
branch_scenario: published/province2015
toc: true
output: html_document
---

The following goal models are from the global assessment in 2014. These models should be modified when better data or indicators are available.

<nav class="navbar navbar-default" role="navigation">   <div class="container-fluid">     <div class="navbar-header">       <a class="navbar-brand" href="#">Branch/Scenario</a>     </div>     <div class="collapse navbar-collapse" id="navbar-1">       <ul class="nav navbar-nav">         <li class="dropdown">           <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">published/province2015<span class="caret"></span></a>           <ul class="dropdown-menu" role="menu">                       <li><a href="{{ site.baseurl }}/draft/province2015/goals/">draft/province2015</a></li>                     </ul>         </li>       </ul>     </div>   </div> </nav> 

<h2>Food Provision: Fisheries</h2>

<p>Amount of sustainable wild-caught seafood compared to the reference multi-species max sustainable yield [\(mMSY_{R}\)]</p>

<p>\[
x_{FIS} =  (1 - \frac{\delta{C_{t}}}{mMSY_{R}})
\]  </p>

<p>\[
\delta{C_{t}} = 
   \begin{cases}
    0                      &\quad \text{if }\ |{mMSY_{r}}-{C_{t}}| < {{0.05}*{mMSY_{r}}}\\
    |{mMSY_{r}}-{C_{t}}|   &\quad \text{if }\ |{mMSY_{r}}-{C_{t}}| < {mMSY_{r}}\\
    {mMSY_{r}}                 &\quad \text{otherwise }
   \end{cases}
\]</p>

<p>\textit{How to calculate mMSY:}
How to calculate mMSY:</p>

<p>\[
B_{t+1} = B_{t} + rB_{t}(1 - \frac{B_{t}}{k}) - C_{t}
\]</p>

<p>\[
C_{t} = qB_{t}f_{t}
\]</p>

<p>\[
B_{t} = U_{t}/q
\]</p>

<p>\[
U_{t} = \frac{C_{t}}{f_{t}}
\]</p>

<p>\[
\frac{U_{t+1}}{q} = \frac{U_{t}}{q} + \frac{rU_{t}}{q[1-\frac{U_{t}}{Kq}]} - U_{t}{f_t}
\]</p>

<p>\[
\frac{U_{t+1}}{U_t} = r - \frac{rU_{t}}{Kq} - qf_{t}
\]</p>

<p>This equation can be converted to a linear regrssion model:
\[
Y = b_{o} + b_{1}x_{1} + b_{2}x_{2}
\]
\(r\),\(q\),\(K\) values can be obtained by solving the linear regression, and thus \(mMSY\)</p>

<p>\[
mMSY = rK/4
\]
\[
mMSY_{r} = 0.75 * mMSY
\]</p>

<ul>
<li><p>\(mMSY_{r}\) = Reference Maxium sustainable yield (\(0.75*mMMSY\))</p></li>
<li><p>\(C\) = total catch at time [\(t\)]</p></li>
<li><p>\(\delta{C_{t}}\) = absolute difference between total landed biomass at time [\(t\)] and mMSY_{r}</p></li>
<li><p>\(B\) = total biomass at time [\(t\)]</p></li>
<li><p>\(r\) = natural growth rate</p></li>
<li><p>\(K\) = carry capacity</p></li>
<li><p>\(C\) = total landed biomass at time [\(t\)]</p></li>
<li><p>\(q\) = catch coefficient</p></li>
<li><p>\(U\) = fishing efforts at time [\(t\)]</p></li>
</ul>

<h2>Food Provision: Mariculture</h2>

<p>Amount of sustainable farm-raised seafood compared to max sustainable</p>

<p>\[
x_{MAR} =  \frac{Y_{c}}{Y_{r}}
\]</p>

<p>\[
Y_{c} = \frac{\sum_{1}^{k} Y_{k} * S_{M,k}}{P_{c}}
\]</p>

<ul>
<li><p>\(Y\) = current sustainably harvested total yield, current [\(c\)] or reference [\(r\)] </p></li>
<li><p>\(S_{M,k}\) = Sustainability score for each species \(k\)</p></li>
<li><p>\(P_{c}\) = Coastal population within 100km</p></li>
<li><p>\(Y_{r} = max(Y_{c})\)</p></li>
</ul>

<h2>Artisanal Fishing Opportunity</h2>

<p>the opportunity to fish artisanally, independent of how many fish are caught</p>

<p>\[
x_{AO} =\frac{\frac{AP_{c}}{AP_{r}} + \frac{AF_{c}}{AF_{r}} + AE_{i} }{3}
\]</p>

<p>\[
AE_{i} = \frac{G_{r}}{I_{r}} - \frac{G_{c}}{I_{c}}
\]</p>

<ul>
<li><p>\(AP\) = number of ports, current [\(c\)] or reference [\(r\)]</p></li>
<li><p>\(AF\) = number of fishermen, current [\(c\)] or reference [\(r\)]</p></li>
<li><p>\(AE\) = economic factor to affect artisinal fishing </p></li>
<li><p>\(G\) = diesel price, current [\(c\)] or reference [\(r\)] </p></li>
<li><p>\(I\) = average yearly disposable income, current [\(c\)] or reference [\(r\)]</p></li>
</ul>

<h2>Natural Products</h2>

<p>Level of protection of the coast from inundation and erosion compared to the local natural potential</p>

<p>\[
x_{NP} = \frac{\sum_{p=1}^{N} w_p * x_p}{N}  
\]</p>

<p>\[
x_p = H_p * S_p
\]</p>

<p>\[
S_p = 1 - (\frac{E + R}{N})  
\]</p>

<ul>
<li><p>\(w_p\) = proportional peak US dollar value of product \(p\)  </p></li>
<li><p>\(x_p\) = sustainable-harvest score for product \(p\)  </p></li>
<li><p>\(H_p\) = harvest yield for product \(p\)  </p></li>
<li><p>\(S_p\) = sustainability of product \(p\) </p></li>
<li><p>\(E\) = exposure term </p></li>
<li><p>\(R\) = risk term</p></li>
<li><p>\(N\) = number of natural products</p></li>
</ul>

<p>￼products:seasalt, marine chemicals, biomedicine</p>

<h2>Carbon Storage</h2>

<p>Extent and condition of coastal habitats that store and sequester atmospheric carbon </p>

<p>\[
x_{CS} =  \sum_{1}^{k} ( c_k * \frac{C_{c}}{C_{r}} * \frac{A_{k}}{A_{T}} )
\]</p>

<ul>
<li><p>\(C\) = condition of habitat \(k\), current [\(c\)] or reference[\(r\)]</p></li>
<li><p>\(A\) = area of habitat \(k\), current [\(c\)] or total area covered by all habitats assessed [T]</p></li>
<li><p>\({c_k}\) = relative contribution to carbon storage by each habitat</p></li>
</ul>

<p>habitats: seagrass beds, salt marshes, mangroves</p>

<h2>Coastal Protection</h2>

<p>Protection from inundation or erosion compared to the local natural potential</p>

<p>\[
x_{CP} =  \sum_{i=1}^{k} ( \frac{C_c}{C_r} * \frac {w_k}{w_max} * \frac{A_k}{A_T} )
\]</p>

<ul>
<li><p>\(C\) = condition of habitat \(k\), current [\(c\)] and reference [\(r\)]</p></li>
<li><p>\(w\) = weighted capacity of habitat \(k\) to protect the coast</p></li>
<li><p>\(A\) = coverage area of habitat \(k\) </p></li>
</ul>

<p>habitats: salt marshes, seagrass beds, mangroves, coral reefs, sea ice</p>

<h2>Tourism &amp; Recreation</h2>

<p>Tourism &amp; recreation value, independent of monetary exchange</p>

<p>\[
x_{TR} = \log (\frac{A_t}{V_t} * {S_t} + {1})
\]</p>

<ul>
<li><p>\(A\) = number of tourists in year \(t\) </p></li>
<li><p>\(V\) = area within jurisdiction </p></li>
<li><p>\(S\) = sustainability coefficient, set as 0.787 (China TTCI, 2009)</p></li>
</ul>

<h2>Livelihoods &amp; Economies: Livelihoods</h2>

<p>Number of jobs and job quality from marine-associated sectors</p>

<p>\[
x_{LIV} = \frac{\frac{\sum_{i=1}^{k} j_{c,k}}{\sum_{i=1}^{k} j_{r,k}} + \frac{g_{c}} {g_{r}}}{2}
\]</p>

<p>\[
j_{c(c, k)} = N_{k} * \frac{x}{\sum{x}}
\]</p>

<ul>
<li><p>\(j\) = number of jobs within sector \(k\) at current [\(c\)] time or reference [\(r\)] level</p></li>
<li><p>\(g\) = sum of average urban and rural family income in current time [\(c\)] or reference [\(r\)] level</p></li>
<li><p>sectors (\(k\)): tourism, commercial fishing, marine mammal watching, aquarium fishing, wave &amp; tidal energy, mariculture, transportation &amp; shipping, ports &amp; harbors, ship &amp; boatbuilding</p></li>
<li><p>\(N\) = total number of employment in each sector [\(k\)] across all regions</p></li>
<li><p>\(x\) = total number of employments across all sectors in each region</p></li>
</ul>

<h2>Livelihoods &amp; Economies: Economies</h2>

<p>Revenue from marine associated sectors</p>

<p>\[
x_{ECO} = \frac{e_{c}}{e_{r}} 
\]</p>

<ul>
<li>\(e\) = total revenue generated from all sectors at current time [\(c\)] or reference [\(r\)] level</li>
</ul>

<h2>Sense of Place: Iconic Species</h2>

<p>Aesthetic connections to and cultural identity with a given region measured through the status of iconic species</p>

<p>\[
x_{ICO} = \frac{\sum_{i=1}^{N} S_{i} * w_{i}}{\sum_{i=1}^{N} S_{i}} 
\]</p>

<ul>
<li><p>\(S\) = number of assessed species in each threat category \(i\) </p></li>
<li><p>\(w\) = status weight assigned per threat category \(i\) </p></li>
<li><p>\(N\) = 6 threat categories </p></li>
</ul>

<p>iconic species list: WWF Priority Species</p>

<p>threat categories: IUCN</p>

<h2>Sense of Place: Lasting Special Places</h2>

<p>Aesthetic connections to and cultural identity with a given region measured through the status of meaningful locations</p>

<p>\[
x_{LSP} = \frac{\%{CMPA}}{\%CMPA_{Ref}} 
\]</p>

<ul>
<li><p>\({\%CMPA}\) = percent protected marine coastal area of the coastal region within jurisdiction</p></li>
<li><p>\({\%CMPA_{Ref}}\) = %5, reference percent protected marine costal area of the coastal region within jurisdiction</p></li>
</ul>

<h2>Clean Waters</h2>

<p>The degree to which coastal waters are free of contaminants</p>

<p>\[
x_{CW} = \sqrt[4]{\prod{a_{i}}}
\]</p>

<ul>
<li><p>\(a\)  = water pollution input</p></li>
<li><p>\(i\) = type of pollution, including phosphate, nitrogen, chemical oxygen demand, and oil pollution</p></li>
</ul>

<h2>Biodiversity: Habitats</h2>

<p>Existence values people hold for biodiversity measured through the status of key habitats</p>

<p>\[
x_{HAB} = \frac{ \sum_{i=1}^{k} \frac{C_{c,k}}{C_{r,k}}}{N}
\]</p>

<ul>
<li><p>\(C\) = condition of habitat k, current [\(c\)] or reference[\(r\)]</p></li>
<li><p>\(N\) = total number of habitat assessed in the region</p></li>
</ul>

<p>Habitats include seagrass beds, salt marshes, mangroves</p>

<h2>Biodiversity: Species</h2>

<p>Existence values people hold for biodiversity measured through the status of native species</p>

<p>\[
x_{SPP} = 1 - \frac{\sum_{i=1}^{N} w_{i}}{N}
\]</p>

<ul>
<li><p>\(N\) = number of assessed species in the province</p></li>
<li><p>\(w_{i}\) = status weight assigned per threat </p></li>
</ul>

<p>assessed species list and maps: IUCN</p>


<script>
// add bootstrap table styles to pandoc tables
$(document).ready(function () {
  $('tr.header').parent('thead').parent('table').addClass('table table-condensed');
});

// dynamically load mathjax for compatibility with self-contained
(function () {
  var script = document.createElement("script");
  script.type = "text/javascript";
  script.src  = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
  document.getElementsByTagName("head")[0].appendChild(script);
})();
</script>

