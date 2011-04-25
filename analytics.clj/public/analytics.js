$.cookie('sid', Math.random());

function reportEvent(event) {
    var sid  = $.cookie('sid');
    var myEvent = { type: event.type,
		    clientX: event.clientX,
		    clientY: event.clientY,
		    timeStamp: event.timeStamp };
    var json = JSON.stringify(myEvent);
    $.get('http://localhost:8080/events/new?sid='+sid+'&event='+json);
    return true;
}

function replaySession(event) {
    var sid  = $.cookie('sid');
    $.get('http://localhost:8080/events/'+sid, function(data) {
	$("#data").html(data);
    });
    return false;
}

TRACKED_EVENTS = ['click', 'mousemove'];

$(document).ready(function() {
    var i;
    for (i = 0; i < TRACKED_EVENTS.length; i++) {
	console.log('Adding listener for ' + TRACKED_EVENTS[i] + ' events');
	$('#canvas').bind(TRACKED_EVENTS[i], reportEvent);
    }

    $('#replay').bind('click', replaySession);
});