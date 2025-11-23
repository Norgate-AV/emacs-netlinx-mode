PROGRAM_NAME='test'

(************ Example ****************)
/*************************************/
#include 'Example.axi'

DEFINE_DEVICE

dvPROJ		=	5001:1:0
dvTP		=	10001:1:0

DEFINE_TYPE
struct _Person {
    integer bar
    char x[100]
}

DEFINE_VARIABLE
volatile char pattern[100] = '/foo/'

define_module 'mExample' example(vdvFoo, dvFoo)

DEFINE_START {
    
}

DEFINE_EVENT

button_event[dvTP, 1] {
    push: {
        send_string dvPROJ, "$02, 'ADZZ;PON', $03"
    }
}
#IF_DEFINED BAR

button_event[dvTP, 2] {
    push: {
        stack_var _Person person
        person.bar = 1;

        send_string dvPROJ, "$02, 'ADZZ;POF', $03"
    }
}

define_function Foo() {
    stack_var integer bar

    bar = 1
    bar = true
    
    for (x = 1; x <= 100; x++) {
        x = find_string()
    }
    foo = mid_string(buf, 1, 2)
}
#END_IF
// foo 
