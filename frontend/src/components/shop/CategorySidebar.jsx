// react
import React, {useEffect, useRef} from 'react';
// third-party
import classNames from 'classnames';
import PropTypes from 'prop-types';
import {connect} from 'react-redux';
// application
import {Cross20Svg} from '../../svg';
import {sidebarClose} from '../../store/sidebar';
// widgets
import WidgetFilters from '../widgets/WidgetFilters';
import WidgetProducts from '../widgets/WidgetProducts';
import {fetchCategories} from "../../store/category";

const getFilters = (categories) => {

    return [{
        id: 1,
        name: 'Categories',
        type: 'categories',
        options: {
            items: categories.map(category => {
                const subCat = category.links.map(subcat => ({
                    id: subcat.id,
                    name: subcat.title,
                    url: subcat.url,
                    type: "child"
                }))
                return [{
                    id: category.id,
                    name: category.title,
                    url: category.url,
                    type: category.links.length === 0 ? "child" : "current",
                }].concat(...subCat)
            }).flat()
        }
    }]
}

function CategorySidebar(props) {
    const {
        sidebarClose,
        sidebarState,
        offcanvas,
        categories,
        fetchCategories,
        products
    } = props;
    if (categories.items.length === 0) {
        fetchCategories()
    }
    const filters = getFilters(categories.items);
    const classes = classNames('block block-sidebar', {
        'block-sidebar--open': sidebarState.open,
        'block-sidebar--offcanvas--always': offcanvas === 'always',
        'block-sidebar--offcanvas--mobile': offcanvas === 'mobile',
    });

    const backdropRef = useRef(null);
    const bodyRef = useRef(null);

    useEffect(() => {
        const media = matchMedia('(max-width: 991px)');
        let changedByMedia = false;

        const onChange = () => {
            if (offcanvas === 'mobile') {
                if (sidebarState.open && !media.matches) {
                    sidebarClose();
                }
                // this is necessary to avoid the transition hiding the sidebar
                if (!sidebarState.open && media.matches && changedByMedia) {
                    /** @var {HTMLElement} */
                    const backdrop = backdropRef.current;
                    /** @var {HTMLElement} */
                    const body = bodyRef.current;

                    backdrop.style.transition = 'none';
                    body.style.transition = 'none';

                    backdrop.getBoundingClientRect(); // force reflow

                    backdrop.style.transition = '';
                    body.style.transition = '';
                }
            }
        };

        if (media.addEventListener) {
            media.addEventListener('change', onChange);
        } else {
            media.addListener(onChange);
        }

        onChange();

        changedByMedia = true;

        return () => {
            if (media.removeEventListener) {
                media.removeEventListener('change', onChange);
            } else {
                media.removeListener(onChange);
            }
        };
    }, [offcanvas, sidebarState.open, sidebarClose]);

    useEffect(() => {
        if (sidebarState.open) {
            const width = document.body.clientWidth;

            document.body.style.overflow = 'hidden';
            document.body.style.paddingRight = `${document.body.clientWidth - width}px`;
        } else {
            document.body.style.overflow = '';
            document.body.style.paddingRight = '';
        }
    }, [sidebarState.open]);

    return (
        <div className={classes}>
            {/* eslint-disable-next-line max-len */}
            {/* eslint-disable-next-line jsx-a11y/click-events-have-key-events, jsx-a11y/no-static-element-interactions */}
            <div className="block-sidebar__backdrop" ref={backdropRef} onClick={() => sidebarClose()}/>
            <div className="block-sidebar__body" ref={bodyRef}>
                <div className="block-sidebar__header">
                    <div className="block-sidebar__title">Filters</div>
                    <button className="block-sidebar__close" type="button" onClick={() => sidebarClose()}>
                        <Cross20Svg/>
                    </button>
                </div>
                <div className="block-sidebar__item">
                    <WidgetFilters title="Filters" filters={filters} offcanvas={offcanvas}/>
                </div>
                {offcanvas !== 'always' && (
                    <div className="block-sidebar__item d-none d-lg-block">
                        <WidgetProducts title="Latest Products" products={products.slice(0, 5)}/>
                    </div>
                )}
            </div>
        </div>
    );
}

CategorySidebar.propTypes = {
    /**
     * indicates when sidebar bar should be off canvas
     */
    offcanvas: PropTypes.oneOf(['always', 'mobile']),
};

CategorySidebar.defaultProps = {
    offcanvas: 'mobile',
};

const mapStateToProps = (state) => ({
    sidebarState: state.sidebar,
    categories: state.categories
});

const mapDispatchToProps = {
    sidebarClose,
    fetchCategories
};

export default connect(mapStateToProps, mapDispatchToProps)(CategorySidebar);
