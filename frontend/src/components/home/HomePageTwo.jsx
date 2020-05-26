// react
import React from 'react';
// third-party
import {Helmet} from 'react-helmet-async';
import {connect} from "react-redux";
// blocks
import BlockBanner from '../blocks/BlockBanner';
// import BlockCategories from '../blocks/BlockCategories';
import BlockProductColumns from '../blocks/BlockProductColumns';
import BlockProducts from '../blocks/BlockProducts';
import BlockSlideShow from '../blocks/BlockSlideShow';
import BlockTabbedProductsCarousel from '../blocks/BlockTabbedProductsCarousel';
// data stubs
import {getStatus, getVisibleProducts} from "../../store/product/productReducer";
import {fetchProducts} from "../../store/product/productActions";

function HomePageTwo(props) {
    const {products, productsStatus, productsBlockA, productsBlockAStatus, productsBlockB, productsBlockBStatus, changeGroup} = props;
    let statusA = products.size > 0 ? productsStatus : productsBlockAStatus
    let statusB = products.size > 0 ? productsStatus : productsBlockBStatus

    const columns = [
        {
            title: 'Top Rated Products',
            products: products.slice(0, 3),
        },
        {
            title: 'Special Off ers',
            products: products.slice(3, 6),
        },
        {
            title: 'Bestsellers',
            products: products.slice(6, 9),
        },
    ];


    return (
        <React.Fragment>
            <Helmet>

                <title>{`FixNBump`}</title>
            </Helmet>

            <BlockSlideShow/>

            {/*<BlockFeatures layout="boxed" />*/}

            <BlockTabbedProductsCarousel status={statusA}
                                         products={productsBlockA.length > 0 ? productsBlockA : products}
                                         changeGroup={id => changeGroup("A", id)}
                                         title="Featured Products" layout="grid-5" rows={1}/>

            <BlockBanner/>
            {products.length > 0 && !productsStatus.error &&
                <BlockProducts
                    title="Bestsellers"
                    layout="large-last"
                    featuredProduct={products[0]}
                    products={products.slice(1, 7)}
                />}

            {/*<BlockCategories title="Popular Categories" layout="compact" categories={categories}/>*/}
            <BlockTabbedProductsCarousel status={statusB}
                                         products={productsBlockB.length > 0 ? productsBlockB : products}
                                         changeGroup={id => changeGroup("B", id)}
                                         title="New Arrivals" layout="grid-5" rows={1}/>

            {/*<BlockPosts title="Latest News" layout="grid-nl" posts={posts} />*/}

            {/*<BlockBrands />*/}

            <BlockProductColumns columns={columns}/>
        </React.Fragment>
    );
}

const mapStateToProps = (state) => {
    return {
        products: getVisibleProducts(state.products),
        productsStatus: getStatus(state.products),
        productsBlockA: getVisibleProducts(state.productsBlockA, "A"),
        productsBlockAStatus: getStatus(state.productsBlockA, "A"),
        productsBlockB: getVisibleProducts(state.productsBlockB, "B"),
        productsBlockBStatus: getStatus(state.productsBlockB, "B"),
    }
}

const mapDispatchToProps = {
    changeGroup: fetchProducts,
};

export default connect(mapStateToProps, mapDispatchToProps)(HomePageTwo);
