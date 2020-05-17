// react
import React, {Component} from 'react';
// third-party
import PropTypes from 'prop-types';
import {BrowserRouter, matchPath, Redirect, Route, Switch} from 'react-router-dom';
import {connect} from 'react-redux';
import {Helmet, HelmetProvider} from 'react-helmet-async';
import {IntlProvider} from 'react-intl';
import {ScrollContext} from 'react-router-scroll-4';
// application
import languages from '../i18n';
import {localeChange} from '../store/locale';
// pages
import Layout from './Layout';
import HomePageTwo from './home/HomePageTwo';
import {fetchProducts} from "../store/product/productActions";


class Root extends Component {
    componentDidMount() {
        // preloader
        setTimeout(() => {
            const preloader = document.querySelector('.site-preloader');

            preloader.addEventListener('transitionend', (event) => {
                if (event.propertyName === 'opacity') {
                    preloader.parentNode.removeChild(preloader);
                }
            });
            preloader.classList.add('site-preloader__fade');
        }, 500);

        // this is for demo only, you can delete it
        const {localeChange: changeLocale} = this.props;
        const direction = new URLSearchParams(window.location.search).get('dir');

        if (direction !== null) {
            changeLocale(direction === 'rtl' ? 'ar' : 'en');
        }
        const shopPath = matchPath(window.location.pathname, {
            path: "/shop/category/:id",
            exact: true,
            strict: false
        });
        console.log(shopPath)
        this.props.fetchProducts("", shopPath?.params?.id ?? "", shopPath?.params?.id ? {limit: 15} : {});
    }

    render() {
        const {locale, products, categories} = this.props;
        const {messages, direction} = languages[locale];

        return (
            <IntlProvider locale={locale} messages={messages}>
                <BrowserRouter>
                    <HelmetProvider>
                        <Helmet htmlAttributes={{lang: locale, dir: direction}}/>
                        <ScrollContext>
                            <Switch>
                                <Route
                                    path="/"
                                    render={(props) => (
                                        <Layout {...props} products={products} categories={categories}
                                                headerLayout="compact" homeComponent={HomePageTwo}/>
                                    )}
                                />
                                <Redirect to="/"/>
                            </Switch>
                        </ScrollContext>
                    </HelmetProvider>
                </BrowserRouter>
            </IntlProvider>
        );
    }
}

Root.propTypes = {
    /** current locale */
    locale: PropTypes.string,
};

const mapStateToProps = (state) => ({
    locale: state.locale
});

const mapDispatchToProps = {
    localeChange,
    fetchProducts
};
export default connect(mapStateToProps, mapDispatchToProps)(Root);
