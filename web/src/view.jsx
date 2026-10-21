import React from "react";

function Inline({ node }) {
  switch (node.kind) {
    case "text":
      return <>{node.value}</>;
    case "code":
      return <code className="view-code">{node.value}</code>;
    case "type":
      return (
        <code className="view-type" data-id={node.id} data-brief={node.brief ? "yes" : "no"}>
          {node.text}
        </code>
      );
    case "variable":
      return <code className="view-variable" data-id={node.id}>{node.name}</code>;
    case "form":
      return <code className="view-form">{node.text}</code>;
    case "value":
      return <code className="view-value" data-id={node.id}>{node.text}</code>;
    case "paragraph":
      return (
        <>
          {node.children.map((child, index) => (
            <Inline key={index} node={child} />
          ))}
        </>
      );
    default:
      return <View node={node} />;
  }
}

export function View({ node }) {
  if (!node) return null;
  switch (node.kind) {
    case "section":
      return (
        <section className="view-section">
          <h3>{node.title}</h3>
          {node.children.map((child, index) => (
            <View key={index} node={child} />
          ))}
        </section>
      );
    case "paragraph":
      return (
        <p className="view-paragraph">
          {node.children.map((child, index) => (
            <Inline key={index} node={child} />
          ))}
        </p>
      );
    case "note":
      return (
        <aside className="view-note">
          {node.children.map((child, index) => (
            <Inline key={index} node={child} />
          ))}
        </aside>
      );
    case "bullets":
      return (
        <ul className="view-list">
          {node.items.map((item, index) => (
            <li key={index}>
              <Inline node={item} />
            </li>
          ))}
        </ul>
      );
    case "steps":
      return (
        <ol className="view-list">
          {node.items.map((item, index) => (
            <li key={index}>
              <Inline node={item} />
            </li>
          ))}
        </ol>
      );
    case "table":
      return (
        <div className="view-table-wrap">
          <table className="view-table">
            <thead>
              <tr>
                {node.columns.map((column) => (
                  <th key={column}>{column}</th>
                ))}
              </tr>
            </thead>
            <tbody>
              {node.rows.map((row, index) => (
                <tr key={index}>
                  {row.map((cell, column) => (
                    <td key={column}>
                      <Inline node={cell} />
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      );
    default:
      return (
        <p className="view-paragraph">
          <Inline node={node} />
        </p>
      );
  }
}
