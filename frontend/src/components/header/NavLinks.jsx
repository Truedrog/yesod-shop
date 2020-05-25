// react
import React, {useEffect, useState} from 'react';

// third-party
import classNames from 'classnames';
import PropTypes from 'prop-types';
import {connect} from 'react-redux';

// application
import AppLink from '../shared/AppLink';
import languages from '../../i18n';
import Megamenu from './Megamenu';
import Menu from './Menu';
import {ArrowRoundedDown9x6Svg} from '../../svg';

// data stubs
// import navLinks from '../../data/headerNavigation';
import {fetchCategories} from "../../store/category";

const generateNavLinks = (categories) => {
    function splitArray(candid) {
        let oddOnes = [],
            evenOnes = [];
        for (let i = 0; i < candid.length; i++)
            (i % 2 === 0 ? evenOnes : oddOnes).push(candid[i]);
        return [evenOnes, oddOnes];
    }

    const [odd, even] = splitArray(categories.items)
    return [
        {
            title: 'Home',
            url: '/',

        },
        {
            title: 'Shop',
            url: '/shop',
            submenu: {
                type: 'megamenu',
                menu: {
                    size: 'nl',
                    columns: [
                        {
                            size: 6,
                            links: odd.map(cat => ({title: cat.title, url: cat.url, id: cat.id,  links: cat.links}))
                        },
                        {
                            size: 6,
                            links: even.map(cat => ({title: cat.title, url: cat.url, id: cat.id, links: cat.links}))
                        },]
                },
            },
        },
    ]
}

function NavLinks(props) {
    const {categories} = props;
    if (categories.items.length === 0 && categories.loading === false) {
        props.fetchCategories();
    }
    const navLinks = generateNavLinks(categories);
    const handleMouseEnter = (event) => {
        const {locale} = props;
        const {direction} = languages[locale];

        const item = event.currentTarget;
        const megamenu = item.querySelector('.nav-links__megamenu');

        if (megamenu) {
            const container = megamenu.offsetParent;
            const containerWidth = container.getBoundingClientRect().width;
            const megamenuWidth = megamenu.getBoundingClientRect().width;
            const itemOffsetLeft = item.offsetLeft;

            if (direction === 'rtl') {
                const itemPosition = containerWidth - (
                    itemOffsetLeft + item.getBoundingClientRect().width
                );

                const megamenuPosition = Math.round(
                    Math.min(itemPosition, containerWidth - megamenuWidth),
                );

                megamenu.style.left = '';
                megamenu.style.right = `${megamenuPosition}px`;
            } else {
                const megamenuPosition = Math.round(
                    Math.min(itemOffsetLeft, containerWidth - megamenuWidth),
                );

                megamenu.style.right = '';
                megamenu.style.left = `${megamenuPosition}px`;
            }
        }
    };

    const linksList = navLinks.map((item, index) => {
        let arrow;
        let submenu;

        if (item.submenu) {
            arrow = <ArrowRoundedDown9x6Svg className="nav-links__arrow"/>;
        }

        if (item.submenu && item.submenu.type === 'menu') {
            submenu = (
                <div className="nav-links__menu">
                    <Menu items={item.submenu.menu}/>
                </div>
            );
        }

        if (item.submenu && item.submenu.type === 'megamenu') {
            submenu = (
                <div className={`nav-links__megamenu nav-links__megamenu--size--${item.submenu.menu.size}`}>
                    <Megamenu menu={item.submenu.menu}/>
                </div>
            );
        }

        const classes = classNames('nav-links__item', {
            'nav-links__item--with-submenu': item.submenu,
        });

        return (
            <li key={index} className={classes} onMouseEnter={handleMouseEnter}>
                <AppLink to={item.url} {...item.props}>
                    <span>
                        {item.title}
                        {arrow}
                    </span>
                </AppLink>
                {submenu}
            </li>
        );
    });

    return (
        <ul className="nav-links__list">
            {linksList}
        </ul>
    );
}

NavLinks.propTypes = {
    /** current locale */
    locale: PropTypes.string,
};

const mapStateToProps = (state) => ({
    locale: state.locale,
    categories: state.categories
});
const mapDispatchToProps = {
    fetchCategories
}
export default connect(mapStateToProps, mapDispatchToProps)(NavLinks);
