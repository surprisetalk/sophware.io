import './main.css';
import './bulma.min.css';
import logoPath from './logo.svg';
const { App } = require('./App.elm');

App.embed(document.getElementById('root'), logoPath);
