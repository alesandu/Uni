const moment = require('moment')

const bl = function(txt){
    console.log(moment().format('YYYY-MM-DD HH:mm:ss - ') + txt)
}
module.exports = bl;