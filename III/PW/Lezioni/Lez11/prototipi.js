function Course(course_name) {
    this.name= course_name;
}

let pw = new Course("Programmazione Web");
pw.__proto__ //={}
Course.prototype // ={}
Course.prototype.university = "Tor Vergata";
pw.university //= "Tor Vergata"

Course.prototype.address = "Via del Politecnico, 1";
pw.address //= "Via del Politecnico, 1"

let pjdm = new Course("Programmazione Java per dispositivi mobili");
pjdm.university //= "Tor Vergata"

pw.__proto__.university = "La Sapienza";
pjdm.university //= "La Sapienza"

//Ho oggetto master che ha una proprietà, se la cambio a master, cambia anche a pw e pjdm