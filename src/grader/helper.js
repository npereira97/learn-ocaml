

function ask(){
	var xhttp = new XMLHttpRequest();
	xhttp.onreadystatechange = function() {
	    if (this.readyState == 4 && this.status == 200) {
	       // Typical action to be performed when the document is ready:
		s = xhttp.responseText
	       console.log(xhttp.responseText);
	    }
	};
	xhttp.open("POST", "http://localhost:5000/", false);
	xhttp.overrideMimeType("text/html");
	xhttp.setRequestHeader('Content-Type', 'text/plain')
	xhttp.send("()");
}
