// react
import React, {Component} from 'react';

// third-party
import classNames from 'classnames';
import PropTypes from 'prop-types';
import {connect} from 'react-redux';

// application
import Pagination from '../shared/Pagination';
import ProductCard from '../shared/ProductCard';
import {
    Filters16Svg,
    LayoutGrid16x16Svg,
    LayoutGridWithDetails16x16Svg,
    LayoutList16x16Svg,
} from '../../svg';
import {sidebarOpen} from '../../store/sidebar';
import {fetchProducts} from "../../store/product/productActions";
import {matchPath} from "react-router-dom";


class ProductsView extends Component {
    constructor(props) {
        super(props);
        this.shopPath = matchPath(window.location.pathname, {
            path: "/shop/category/:id",
            exact: true,
            strict: false
        });
        this.state = {
            page: 1,
            perPage: 12,
            total: 0
        };
    }

    async componentDidMount() {
        const res = await fetch("/api/products/count");
        const json = await res.json();
        this.setState(() => ({total: json.result}))
    }

    setLayout = (layout) => {
        this.setState(() => ({layout}));
    };

    handlePageChange = (page) => {
        this.setState(() => ({page}), () => {
            let offset = (this.state.page - 1) * this.state.perPage;
            this.props.fetchProducts("", this.shopPath?.params?.id ?? "", {
                limit: this.state.perPage,
                offset: offset,
                sort: "desc"
            });
        });
    };

    changePerPage = (event) => {
        this.setState({perPage: event.target.value}, () => {
            let limit = (this.state.page - 1) * this.state.perPage;
            this.props.fetchProducts("", this.shopPath?.params?.id ?? "", {
                limit: limit,
                sort: "desc"
            });
        });
    }

    render() {
        const {
            products,
            grid,
            offcanvas,
            layout: propsLayout,
            sidebarOpen,
        } = this.props;
        const {page, layout: stateLayout} = this.state;
        const layout = stateLayout || propsLayout;

        let viewModes = [
            {key: 'grid', title: 'Grid', icon: <LayoutGrid16x16Svg/>},
            {key: 'grid-with-features', title: 'Grid With Features', icon: <LayoutGridWithDetails16x16Svg/>},
            {key: 'list', title: 'List', icon: <LayoutList16x16Svg/>},
        ];

        viewModes = viewModes.map((viewMode) => {
            const className = classNames('layout-switcher__button', {
                'layout-switcher__button--active': layout === viewMode.key,
            });

            return (
                <button
                    key={viewMode.key}
                    title={viewMode.title}
                    type="button"
                    className={className}
                    onClick={() => this.setLayout(viewMode.key)}
                >
                    {viewMode.icon}
                </button>
            );
        });

        const productsList = products.map((product) => (
            <div key={product.id} className="products-list__item">
                <ProductCard product={product}/>
            </div>
        ));

        const viewOptionsClasses = classNames('view-options', {
            'view-options--offcanvas--always': offcanvas === 'always',
            'view-options--offcanvas--mobile': offcanvas === 'mobile',
        });

        return (
            <div className="products-view">
                <div className="products-view__options">
                    <div className={viewOptionsClasses}>
                        <div className="view-options__filters-button">
                            <button type="button" className="filters-button" onClick={() => sidebarOpen()}>
                                <Filters16Svg className="filters-button__icon"/>
                                <span className="filters-button__title">Filters</span>
                                {/*<span className="filters-button__counter">3</span>*/}
                            </button>
                        </div>
                        <div className="view-options__layout">
                            <div className="layout-switcher">
                                <div className="layout-switcher__list">
                                    {viewModes}
                                </div>
                            </div>
                        </div>
                        <div className="view-options__legend">
                            {`Showing ${this.state.perPage} of ${this.state.total} products`}
                        </div>
                        <div className="view-options__divider"/>
                        <div className="view-options__control">
                            <label htmlFor="view-options-sort">Sort By</label>
                            <div>
                                <select className="form-control form-control-sm" name="" id="view-options-sort">
                                    <option value="">Default</option>
                                    <option value="">Name (A-Z)</option>
                                </select>
                            </div>
                        </div>
                        <div className="view-options__control">
                            <label htmlFor="view-options-limit">Show</label>
                            <div>
                                <select className="form-control form-control-sm" name="" id="view-options-limit"
                                        value={this.state.perPage}
                                        onChange={this.changePerPage}>
                                    <option value="12">12</option>
                                    <option value="24">24</option>
                                </select>
                            </div>
                        </div>
                    </div>
                </div>

                <div
                    className="products-view__list products-list"
                    data-layout={layout !== 'list' ? grid : layout}
                    data-with-features={layout === 'grid-with-features' ? 'true' : 'false'}
                >
                    <div className="products-list__body">
                        {productsList}
                    </div>
                </div>

                <div className="products-view__pagination">
                    <Pagination
                        current={page}
                        siblings={2}
                        total={Math.ceil(this.state.total / this.state.perPage)}
                        onPageChange={this.handlePageChange}
                    />
                </div>
            </div>
        );
    }
}

ProductsView.propTypes = {
    /**
     * array of product objects
     */
    products: PropTypes.array,
    /**
     * products list layout (default: 'grid')
     * one of ['grid', 'grid-with-features', 'list']
     */
    layout: PropTypes.oneOf(['grid', 'grid-with-features', 'list']),
    /**
     * products list layout (default: 'grid')
     * one of ['grid-3-sidebar', 'grid-4-full', 'grid-5-full']
     */
    grid: PropTypes.oneOf(['grid-3-sidebar', 'grid-4-full', 'grid-5-full']),
    /**
     * indicates when sidebar bar should be off canvas
     */
    offcanvas: PropTypes.oneOf(['always', 'mobile']),
};

ProductsView.defaultProps = {
    products: [],
    layout: 'grid',
    grid: 'grid-3-sidebar',
    offcanvas: 'mobile',
};

const mapDispatchToProps = {
    sidebarOpen,
    fetchProducts
};

export default connect(
    () => ({}),
    mapDispatchToProps,
)(ProductsView);
