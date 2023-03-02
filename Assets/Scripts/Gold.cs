using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
public class Gold : MonoBehaviour
{
    public Text gold;

    private void Update()
    {
        gold.text = "$" + Stats.money.ToString();    
    }

}
