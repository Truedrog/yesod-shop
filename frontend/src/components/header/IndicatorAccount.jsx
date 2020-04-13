// react
import React from 'react';

// third-party
import { Link } from 'react-router-dom';

// application
import Indicator from './Indicator';
import { Person20Svg } from '../../svg';


export default function IndicatorAccount() {
    const dropdown = (
        <div className="account-menu">
            <form className="account-menu__form">
                <div className="account-menu__form-title">Log In to Your Account</div>
                <div className="form-group">
                    <label htmlFor="header-signin-email" className="sr-only">Email address</label>
                    <input
                        id="header-signin-email"
                        type="email"
                        className="form-control form-control-sm"
                        placeholder="Email address"
                    />
                </div>
                <div className="form-group">
                    <label htmlFor="header-signin-password" className="sr-only">Password</label>
                    <div className="account-menu__form-forgot">
                        <input
                            id="header-signin-password"
                            type="password"
                            className="form-control form-control-sm"
                            placeholder="Password"
                        />
                        <Link to="/account/login" className="account-menu__form-forgot-link">Forgot?</Link>
                    </div>
                </div>
                <div className="form-group account-menu__form-button">
                    <button type="submit" className="btn btn-primary btn-sm">Login</button>
                </div>
                <div className="account-menu__form-link">
                    <Link to="/account/login">Create An Account</Link>
                </div>
            </form>
            <div className="account-menu__divider" />
            <Link to="/account/dashboard" className="account-menu__user">
                <div className="account-menu__user-avatar">
                    <img src="images/avatars/avatar-3.jpg" alt="" />
                </div>
                <div className="account-menu__user-info">
                    <div className="account-menu__user-name">Helena Garcia</div>
                    <div className="account-menu__user-email">stroyka@example.com</div>
                </div>
            </Link>
            <div className="account-menu__divider" />
            <ul className="account-menu__links">
                <li><Link to="/account/profile">Edit Profile</Link></li>
                <li><Link to="/account/orders">Order History</Link></li>
                <li><Link to="/account/addresses">Addresses</Link></li>
                <li><Link to="/account/password">Password</Link></li>
            </ul>
            <div className="account-menu__divider" />
            <ul className="account-menu__links">
                <li><Link to="/account/login">Logout</Link></li>
            </ul>
        </div>
    );

    return (
        <Indicator url="/account" dropdown={dropdown} icon={<Person20Svg />} />
    );
}
