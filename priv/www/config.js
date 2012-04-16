var login_systems = {};
var configs = ["K2 Informatics", "Swisscom", "Mock DB"];

login_systems[configs[0]] = {ip         :"192.168.1.69",
                             port       :"1521",
                             service    :"SBS0.k2informatics.ch",
                             type       :"service",
                             user       :"SBS0",
                             password   :"sbs0sbs0_4dev"};
login_systems[configs[1]] = {ip         :"10.132.26.159",
                             port       :"3520",
                             service    :"SBS1",
                             type       :"sid",
                             user       :"SBS0",
                             password   :"sbs0sbs1_4test"}; //SBS1.TEST
login_systems[configs[2]] = {ip         :"127.0.0.1",
                             port       :"1521",
                             service    :"MOCK",
                             type       :"service",
                             user       :"dba",
                             password   :"supersecret"};
