    function putRating(ratingObject, result, label) {
        ratingObject.raty("readOnly", true);
        $.ajax({
            type: "PUT",
            url: "/api/label/" + encodeURIComponent(result.query) + "/" + encodeURIComponent(result.formula) + "/" + encodeURIComponent(result.url) + "/" + encodeURIComponent(label)
            }).done(function(data) {
            if (data.status == "OK") { 
                result.label = label;
                ratingObject.raty("readOnly", false);
            }
            else {
                setRating(ratingObject, result.label);
                error(data.message);
            }
          })
          .fail(
          function( jqxhr, textStatus, e ) {
                setRating(ratingObject, result.label);
                var err = textStatus + ', ' + e;
                error("Request Failed: " + err);
            });
    }
    
    function getRating(ratingObject, result) {
        ratingObject.raty("readOnly", true);
        var url = "/api/label/" + encodeURIComponent(result.query) + "/" + encodeURIComponent(result.formula) + "/" + encodeURIComponent(result.url);
        $.ajax({
            type: "GET",
            url: url
            }).done(function(data) {
            if (data.status == "OK") {
                result.label = data.label;
                setRating(ratingObject, data.label);
            }
            else {
                ratingObject.raty("readOnly", false);
                error(data.message);
            }
          })
          .fail(
          function( jqxhr, textStatus, e ) {
                ratingObject.raty("readOnly", false);
                var err = textStatus + ', ' + e;
                error("Request Failed: " + err);
            });
    }
    
    function deleteRating(ratingObject, result) {
        ratingObject.raty("readOnly", true);
        $.ajax({
            type: "DELETE",
            url: "/api/label/" + encodeURIComponent(result.query) + "/" + encodeURIComponent(result.formula) + "/" + encodeURIComponent(result.url)
            }).done(function(data) {
            if (data.status == "OK") {
                result.label = -1;
                setRating(ratingObject, -1);
            }
            else {
                setRating(ratingObject, result.label);
                error(data.message);
            }
          })
          .fail(
          function( jqxhr, textStatus, e ) {
                setRating(ratingObject, result.label);
                var err = textStatus + ', ' + e;
                error("Request Failed: " + err);
            });
    }
    
    function setRating(ratingObject, label) {
        ratingObject.raty("readOnly", false);
        if(label && label >= 0) {
            ratingObject.raty("setScore", label);
        }
        else {
            ratingObject.raty("cancel");
        }
    }