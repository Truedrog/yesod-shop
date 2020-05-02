// react
import React, { Component } from 'react';

// third-party
import PropTypes from 'prop-types';

// application
// import products from '../../data/shopProducts';

// data stubs
import BlockProductsCarousel from './BlockProductsCarousel';

export default class BlockTabbedProductsCarousel extends Component {

    constructor(props) {
        super(props);
        this.state = {
            groups: [
                { id: 0, name: 'All', current: true },
                { id: 1, name: 'Power Tools', current: false },
                { id: 6, name: 'Hand Tools', current: false },
                { id: 9, name: 'Tool Storage', current: false },
            ],
        };
    }

    handleChangeGroup = (newCurrentGroup) => {

        const {changeGroup} = this.props;

        const { groups } = this.state;
        const currentGroup = groups.find((group) => group.current);

        if (currentGroup && currentGroup.id === newCurrentGroup.id) {
            return;
        }

        this.setState((state) => (
            {
                groups: state.groups.map((group) => (
                    { ...group, current: group.id === newCurrentGroup.id }
                )),
            }
        ));
        changeGroup(newCurrentGroup.id)
    };

    render() {
        return (
            <BlockProductsCarousel
                {...this.props}
                {...this.state}
                rows={1}
                onGroupClick={this.handleChangeGroup}
            />
        );
    }
}

BlockTabbedProductsCarousel.propTypes = {
    products: PropTypes.object,
    changeUrl: PropTypes.func,
    title: PropTypes.string.isRequired,
    layout: PropTypes.oneOf(['grid-4', 'grid-4-sm', 'grid-5', 'horizontal']),
    rows: PropTypes.number,
    withSidebar: PropTypes.bool,
};

BlockTabbedProductsCarousel.defaultProps = {
    layout: 'grid-4',
    rows: 1,
    withSidebar: false,
};
