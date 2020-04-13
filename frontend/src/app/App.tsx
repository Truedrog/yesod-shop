import React from 'react';
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Link
} from "react-router-dom";

import logo from '../logo.svg';
import './App.css';
import Home from "../home/Home"
import Dashboard from '../dashboard/Dashboard';
import About from '../about/About';

const App: React.FC = () => {
  return (
    <Router>
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <a
            className="App-link"
            href="https://reactjs.org"
            target="_blank"
            rel="noopener noreferrer"
          >
            Learn React
          </a>
          <ul>
            <li>
              <Link to="" className="App-link">Home</Link>
              <Link to="/" className="App-link">Home</Link>
            </li>
            <li>
              <Link to="/about" className="App-link">About</Link>
            </li>
            <li>
              <Link to="/dashboard" className="App-link">Dashboard</Link>
            </li>
          </ul>
        </header>

        <hr />

        {/*
          A <Switch> looks through all its children <Route>
          elements and renders the first one whose path
          matches the current URL. Use a <Switch> any time
          you have multiple routes, but you want only one
          of them to render at a time
        */}
        <Switch>
          <Route exact path="/">
            <Home />
          </Route>
          <Route path="/about">
            <About />
          </Route>
          <Route path="/dashboard">
            <Dashboard />
          </Route>
        </Switch>
      </div>
    </Router>
    // <div className="App">

    // </div>
  );
}

export default App;

