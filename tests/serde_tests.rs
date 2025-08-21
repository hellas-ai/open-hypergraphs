#[cfg(feature = "serde")]
mod serde_tests {
    use open_hypergraphs::lax::OpenHypergraph;
    use serde_json;

    #[test]
    fn test_open_hypergraph_serialization() {
        // Create a simple open hypergraph
        let mut graph: OpenHypergraph<String, String> = OpenHypergraph::singleton(
            "add".to_string(),
            vec!["int".to_string(), "int".to_string()],
            vec!["int".to_string()],
        );

        // Add another operation
        let (_, (_sources, _targets)) = graph.new_operation(
            "multiply".to_string(),
            vec!["int".to_string()],
            vec!["int".to_string()],
        );

        // Serialize to JSON
        let json = serde_json::to_string(&graph).expect("Serialization should succeed");
        assert!(!json.is_empty());
        assert!(json.contains("add"));
        assert!(json.contains("multiply"));
        assert!(json.contains("int"));

        // Deserialize back
        let deserialized: OpenHypergraph<String, String> =
            serde_json::from_str(&json).expect("Deserialization should succeed");

        // Verify the deserialized graph matches the original
        assert_eq!(graph.sources.len(), deserialized.sources.len());
        assert_eq!(graph.targets.len(), deserialized.targets.len());
        assert_eq!(
            graph.hypergraph.nodes.len(),
            deserialized.hypergraph.nodes.len()
        );
        assert_eq!(
            graph.hypergraph.edges.len(),
            deserialized.hypergraph.edges.len()
        );
        assert_eq!(
            graph.hypergraph.adjacency.len(),
            deserialized.hypergraph.adjacency.len()
        );

        // Check specific values
        assert_eq!(graph.sources, deserialized.sources);
        assert_eq!(graph.targets, deserialized.targets);
        assert_eq!(graph.hypergraph.nodes, deserialized.hypergraph.nodes);
        assert_eq!(graph.hypergraph.edges, deserialized.hypergraph.edges);
    }

    #[test]
    fn test_empty_open_hypergraph_serialization() {
        let graph: OpenHypergraph<String, String> = OpenHypergraph::empty();

        // Serialize
        let json = serde_json::to_string(&graph).expect("Serialization should succeed");

        // Deserialize
        let deserialized: OpenHypergraph<String, String> =
            serde_json::from_str(&json).expect("Deserialization should succeed");

        // Verify empty state
        assert!(deserialized.sources.is_empty());
        assert!(deserialized.targets.is_empty());
        assert!(deserialized.hypergraph.nodes.is_empty());
        assert!(deserialized.hypergraph.edges.is_empty());
        assert!(deserialized.hypergraph.adjacency.is_empty());
    }

    #[test]
    fn test_hypergraph_with_quotient_serialization() {
        let mut graph: OpenHypergraph<String, String> = OpenHypergraph::singleton(
            "op1".to_string(),
            vec!["type1".to_string()],
            vec!["type2".to_string()],
        );

        // Add nodes and create a quotient
        let node1 = graph.new_node("type1".to_string());
        let node2 = graph.new_node("type1".to_string());
        graph.unify(node1, node2);

        // Serialize and deserialize
        let json = serde_json::to_string(&graph).expect("Serialization should succeed");
        let deserialized: OpenHypergraph<String, String> =
            serde_json::from_str(&json).expect("Deserialization should succeed");

        // Verify quotient map is preserved
        assert_eq!(
            graph.hypergraph.quotient.0.len(),
            deserialized.hypergraph.quotient.0.len()
        );
        assert_eq!(
            graph.hypergraph.quotient.1.len(),
            deserialized.hypergraph.quotient.1.len()
        );
        assert_eq!(graph.hypergraph.quotient, deserialized.hypergraph.quotient);
    }
}
