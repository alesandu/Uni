const moment = require('moment');
const dotenv = require('dotenv');
const chalk = require('chalk');
const bl = require('betterlog');

bl("Ciao Log 1");

console.log(chalk.red('Hello world!'));

dotenv.config();
//console.log(process.env);
//console.log(process.env.APIKEY);

const now = moment();

console.log(now.format('YYYY-MM-DD HH:mm:ss'));