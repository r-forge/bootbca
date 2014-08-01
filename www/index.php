
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

<!-- get project title  -->
<h1>Welcome to the bootBCa project!</h1>
<!-- own website starts here, the following may be changed as you like -->

<p>The package bootBCa exports a single function, BCa, which finds
confidence intervals using Efron's nonparametric bias-corrected and
accelerated (BC<sub><i>a</i></sub>) bootstrap method.&nbsp; It is an enhanced
derivative of the function bcanon, forked from <a
href="http://cran.r-project.org/web/packages/bootstrap/">bootstrap_2012.04-1</a>.&nbsp;
Adaptive determination of the number of bootstrap replications is supported
and the amount of memory required is less by a factor of nboot.</p>

<p>Although the BCa function does not parallelize internally, multiple
invocations of the function can profitably be run in parallel on different
data (e.g., different treatments of an experiment).&nbsp; One script that
does this is <a href="https://rubygems.org/gems/GECS">GECS (Gem for
Experimental Computer Science)</a>.</p>

<p>For details please see the package documentation, which is linked here in <a href="manual/BCa.html">HTML</a> and <a href="bootBCa-manual.pdf">PDF</a> formats and included in the package in Rd format.</p>

<p>The package can be downloaded from <a
href="http://r-forge.r-project.org/R/?group_id=1928">here</a> or installed
directly with the R command <code>install.packages("bootBCa", repos="http://R-Forge.R-project.org")</code>.</p>

<hr/>

<p>Back to <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name;?>/">R-Forge project summary page</a> for bootBCa</p>

</body>
</html>
