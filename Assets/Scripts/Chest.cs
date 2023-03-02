using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Chest : MonoBehaviour
{
    private void OnMouseDown()
    {
        Stats.money += 25;
        Destroy(gameObject);
    }
}
