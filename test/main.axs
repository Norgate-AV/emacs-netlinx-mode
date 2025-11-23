PROGRAM_NAME='test'

DEFINE_DEVICE

dvPROJ		=	5001:1:0
dvTP		=	10001:1:0


DEFINE_EVENT

button_event[dvTP, 1] {
    push: {
        send_string dvPROJ, "$02, 'ADZZ;PON', $03"
    }
}

button_event[dvTP, 2] {
    push: {
        send_string dvPROJ, "$02, 'ADZZ;POF', $03"
    }
}